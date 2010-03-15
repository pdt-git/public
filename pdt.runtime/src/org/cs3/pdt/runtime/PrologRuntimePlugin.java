/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.runtime;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.WeakHashMap;

import org.cs3.pdt.runtime.internal.DefaultPrologContextTrackerService;
import org.cs3.pdt.runtime.internal.DefaultSAXPrologInterfaceRegistry;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.BootstrapPrologContribution;
import org.cs3.pl.prolog.DefaultPrologLibrary;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceConstants;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibrary;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.UDPEventDispatcher;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.PreferenceProvider;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.util.BundleUtility;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PrologRuntimePlugin extends AbstractUIPlugin implements IStartup {

	public class EclipsePreferenceProvider implements PreferenceProvider {

		@Override
		public String getPreference(String key) {
			return plugin.overridePreferenceBySystemProperty(key);
		}
	}

	// The shared instance.
	private static PrologRuntimePlugin plugin;
	// Resource bundle.
	private ResourceBundle resourceBundle;

	private DefaultSAXPrologInterfaceRegistry registry;
	private static PrologLibraryManager libraryManager;
	private DefaultResourceFileLocator resourceLocator;
	private PrologContextTrackerService contextTrackerService;
	private HashMap<String, Map> globalHooks;
	private Map<String, List<BootstrapPrologContribution>> bootStrapContribForKey;
	private Map<String, BootstrapPrologContribution> allBootStrapLists = new HashMap<String, BootstrapPrologContribution>();
	private WeakHashMap<PrologInterface, IPrologEventDispatcher> dispatchers = new WeakHashMap<PrologInterface, IPrologEventDispatcher>();
	private HashSet<RegistryHook> registryHooks = new HashSet<RegistryHook>();

	private final static Object contextTrackerMux = new Object();
	private final static Object libraryManagerMux = new Object();
	private final static Object globalHooksMux = new Object();
	private final static Object registryMux = new Object();
	private static final Object preferencesMux = new Object();

	public PrologRuntimePlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle.getBundle("prg.cs3.pdt.runtime.PrologRuntimePluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	/**
	 * Returns the shared instance.
	 */
	public static PrologRuntimePlugin getDefault() {
		if (plugin == null)
			plugin = new PrologRuntimePlugin();
		return plugin;
	}

	/**
	 * Returns the string from the plugin's resource bundle, or 'key' if not
	 * found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle = getDefault().getResourceBundle();
		try {
			return (bundle != null) ? bundle.getString(key) : key;
		} catch (MissingResourceException e) {
			return key;
		}
	}

	public static PrologLibraryManager getLibraryManager() {
		synchronized (libraryManagerMux) {
			if (libraryManager == null) {

				libraryManager = new PrologLibraryManager();
				registerStaticLibraries();

			}

			return libraryManager;
		}
	}
	
	public static String guessFileSearchPath(String libraryId) {
		PrologLibraryManager mgr = getLibraryManager();
		if (mgr == null) {
			return null;
		}
		PrologLibrary lib = mgr.resolveLibrary(libraryId);
		if (lib == null) {
			return null;
		}
		return "library=" + lib.getPath();
	}	

	public ResourceFileLocator getResourceLocator() {
		if (resourceLocator == null){
			String bootStrapDir = getPreferenceValue(PrologRuntime.PREF_PIF_BOOTSTRAP_DIR, System.getProperty("java.io.tmpdir"));
			resourceLocator = new DefaultResourceFileLocator(new File(bootStrapDir));			
		}
		return resourceLocator;
	}
	
	public File ensureInstalled(String res, Class<?> clazz) {
		File f = getResourceLocator().resolve(res);
		if (f.exists()) {
			f.delete();
		}
		if (!f.exists()) {
			f.getParentFile().mkdirs();
			try {
				BufferedOutputStream out = new BufferedOutputStream(new FileOutputStream(f));
				InputStream in = clazz.getResourceAsStream(res);
				Util.copy(in, out);
				in.close();
				out.close();
			} catch (IOException e) {
				Debug.rethrow(e);
			}
		}
		return f;
	}

	
	public PrologContextTrackerService getContextTrackerService() {
		synchronized (contextTrackerMux) {
			if (contextTrackerService == null) {
				contextTrackerService = new DefaultPrologContextTrackerService();
				registerStaticTrackers();
			}
			return contextTrackerService;
		}
	}

	public void earlyStartup() {
		/*
		 * if the context tracker service was not requested yet, it does not
		 * contain any then we do not have to initialize the registered
		 * trackers. (the trackers will be initialized when they get registered)
		 * 
		 * But if trackers were registered before the ui was up, we have to
		 * initialize them now.
		 */
		if (contextTrackerService == null) {
			return;
		}
		final IWorkbench workbench = PlatformUI.getWorkbench();
		workbench.getDisplay().asyncExec(new Runnable() {
			public void run() {
				PrologContextTracker[] contextTrackers = contextTrackerService.getContextTrackers();
				for (int i = 0; i < contextTrackers.length; i++) {
					contextTrackers[i].init(workbench);
				}
			}
		});
	}


	
	private PrologInterface createPrologInterface(String name) {
		PrologInterface prologInterface = null;
		

		prologInterface = AbstractPrologInterface.newInstance(AbstractPrologInterface.PL_INTERFACE_DEFAULT,name);
		prologInterface.initOptions(new EclipsePreferenceProvider());
		
		prologInterface.setFileSearchPath(guessFileSearchPath("pdt.runtime.socket.codebase"));
		return prologInterface;
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	/**
	 * look up a preference value.
	 * <p>
	 * will return user settings if available or default settings if not. If a
	 * system property with the given key is defined it will overrule any
	 * existing setting in the preference store. if the key is not defined, this
	 * method returns the given default..
	 * 
	 * @param key
	 * @return the value or specified default if no such key exists..
	 */
	public String getPreferenceValue(String key, String defaultValue) {

		synchronized (preferencesMux) {

			String value = getPreferenceStore().getString(key);

			return System.getProperty(key, value);
		}
	}

	public void reconfigure() {
		reconfigurePrologInterfaces();
	}

	private void reconfigurePrologInterfaces() {
		PrologInterfaceRegistry r = getPrologInterfaceRegistry();
		Set<String> keys = r.getRegisteredKeys();
		for (Iterator<String> it = keys.iterator(); it.hasNext();) {
			String key = it.next();
			PrologInterface pif = r.getPrologInterface(key);
			initPrologInterfaceOptions(pif);
		}
	}

	private void initPrologInterfaceOptions(PrologInterface prologInterface) {
		prologInterface.initOptions(new EclipsePreferenceProvider());
	}


	private Map<String, List<BootstrapPrologContribution>> getBootStrapLists() {
		if (bootStrapContribForKey == null) {
			bootStrapContribForKey = new HashMap<String, List<BootstrapPrologContribution>>();
			registerStaticBootstrapContributions();
		}
		return bootStrapContribForKey;
	}

	private static void registerStaticLibraries() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologInterfaceConstants.PLUGIN_ID, PrologRuntime.EP_PROLOG_LIBRARY);
		if (point == null) {
			Debug.error("could not find the extension point " + PrologRuntime.EP_PROLOG_LIBRARY);
			throw new RuntimeException("could not find the extension point " + PrologRuntime.EP_PROLOG_LIBRARY);
		}
		IExtension[] extensions = point.getExtensions();

		for (int i = 0; i < extensions.length; i++) {
			IExtension ext = extensions[i];
			IConfigurationElement[] configurationElements = ext.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement elm = configurationElements[j];

				String id = elm.getAttribute("id");
				String alias = elm.getAttribute("alias");
				String resName = elm.getAttribute("path");
				Debug.debug("got this resname: " + resName);
				String namespace = ext.getContributor().getName();

				Debug.debug("got this namespace: " + namespace);

				URL url = BundleUtility.find(Platform.getBundle(namespace), resName);
				try {

					Debug.debug("trying to resolve this url: " + url);
					url = FileLocator.toFileURL(url);
				} catch (Exception e) {
					Debug.rethrow("Problem resolving url: " + url.toString(), e);
				}
				File file = new File(url.getFile());
				String path = Util.prologFileName(file);

				IConfigurationElement[] childElms = elm.getChildren();
				Map<String, String> libAttrs = new HashMap<String, String>();
				Set<String> deps = new HashSet<String>();
				for (int k = 0; k < childElms.length; k++) {
					IConfigurationElement childElm = childElms[k];
					if ("dependency".equals(childElm.getName())) {
						deps.add(childElm.getAttribute("library"));
					} else if ("attribute".equals(childElm.getName())) {
						libAttrs.put(childElm.getAttribute("id"), childElm.getAttribute("value"));
					} else {
						Debug.warning("within <library> element, i found an unexpected child element: " + childElm.getName());
					}
				}

				PrologLibrary lib = new DefaultPrologLibrary(id, deps, alias, path, libAttrs);
				getLibraryManager().addLibrary(lib);

			}
		}
	}

	private void registerStaticBootstrapContributions() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologInterfaceConstants.PLUGIN_ID, PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);

		if (point == null) {
			Debug.error("could not find the extension point " + PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
			throw new RuntimeException("could not find the extension point " + PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
		}

		for (IExtension extension : point.getExtensions()) {
			registerBootstrapContribution(extension);
		}

		for (String key : bootStrapContribForKey.keySet()) {
			HashSet<BootstrapPrologContribution> contributions = new HashSet<BootstrapPrologContribution>(bootStrapContribForKey.get(key));
			
			int lastLen;
			do {
				lastLen = contributions.size();
				for (BootstrapPrologContribution contribution : new ArrayList<BootstrapPrologContribution>(contributions)) {
					for (String dependency : contribution.getDependencies()) {
						BootstrapPrologContribution depContribution = allBootStrapLists.get(dependency );
						if(depContribution == null){
							Debug.error("dependency does not exist: " + dependency +", for contribution " + contribution.getId());
						} else {
							contributions.add(depContribution);
						}
					}
				}
			} while(lastLen < contributions.size());
			
			
			List<BootstrapPrologContribution> sortedContributions = new ArrayList<BootstrapPrologContribution>();
			List<BootstrapPrologContribution> fileContribs = new ArrayList<BootstrapPrologContribution>();

			separateContributions(contributions,sortedContributions,fileContribs);

			topologicalSort(fileContribs);
			sortedContributions.addAll(fileContribs);
			Debug.info("==== Sorted bsc list for key: " + key + " =====");
			for (BootstrapPrologContribution bootstrapPrologContribution : sortedContributions) {
				Debug.info(" - " + bootstrapPrologContribution);
			}
			bootStrapContribForKey.put(key, sortedContributions);
		}
	}
	
		private void separateContributions(
			HashSet<BootstrapPrologContribution> allContribs, List<BootstrapPrologContribution> pathContribs, List<BootstrapPrologContribution> fileContribs) {
		for (BootstrapPrologContribution contribution : allContribs) {
			if(contribution instanceof BootstrapPrologContributionFile){
				fileContribs.add(contribution);
			} else {
				pathContribs.add(contribution);
			}
		}
	}
	

	private class ContributionPredecessors {

		BootstrapPrologContribution contribution;
		int numPredecessors = 0;

		public ContributionPredecessors(BootstrapPrologContribution contribution) {
			this.contribution = contribution;
		}

	}

	private void topologicalSort(List<BootstrapPrologContribution> contributions) {
		Map<String, ContributionPredecessors> predecessors = new HashMap<String, ContributionPredecessors>();
		initPredecessors(contributions, predecessors);
		List<String> remove = new ArrayList<String>();
		topologicalSorting(contributions, predecessors, remove);
		if (predecessors.keySet().size() > 0) {
			throw new RuntimeException("cycle found in bootstrap contribution dependencies: " + contributions);
		}

	}

	private void initPredecessors(
			List<BootstrapPrologContribution> contributions,
			Map<String, ContributionPredecessors> predecessors) {
		for (BootstrapPrologContribution contribution : contributions) {
			ContributionPredecessors current = predecessors.get(contribution.getId());
			if (current != null) {
				throw new RuntimeException("Two bootstrap contributions have the same id: " + contribution.getId());
			}
			current = new ContributionPredecessors(contribution);
			predecessors.put(contribution.getId(), current);
		}
		for (BootstrapPrologContribution contribution : contributions) {
			for (String dependencyId : contribution.getDependencies()) {
				if(allBootStrapLists.get(dependencyId) instanceof BootstrapPrologContributionFile){
					ContributionPredecessors contributionPredecessors = predecessors.get(dependencyId);
					contributionPredecessors.numPredecessors++;
				}
			}
		}
	}
	
	private void topologicalSorting(
			List<BootstrapPrologContribution> contributions,
			Map<String, ContributionPredecessors> predecessors,
			List<String> remove) {
		int counter = 0;
		contributions.clear();
		while (!predecessors.isEmpty() && counter <= contributions.size()) {
			counter++;
			for (String id : predecessors.keySet()) {
				ContributionPredecessors contrib = predecessors.get(id);
				if (contrib.numPredecessors == 0) {
					contributions.add(0, contrib.contribution);
					remove.add(id);
				}
			}
			for (String id : remove) {
				for (String depId : predecessors.get(id).contribution.getDependencies()) {
					if(predecessors.get(depId) != null)// otherwise it will be an alias
						predecessors.get(depId).numPredecessors--;
				}
				predecessors.remove(id);
			}
			remove.clear();
		}
	}

	private void registerBootstrapContribution(IExtension extension) {
		for (IConfigurationElement element : extension.getConfigurationElements()) {
			try {
				addBootstrap(extension, element);
			} catch (RuntimeException e) {
				/*
				 * DO nothing. Throwing here breaks the "Safe Platform Rule".
				 * The exception is already entered into the PDT log!
				 */
			}
		}
	}

	private void addBootstrap(IExtension extension, IConfigurationElement element) {

		String contributionKey = element.getAttribute("key");
		String contributionId = element.getAttribute("id");
		contributionKey = (contributionKey == null) ? "" : contributionKey;

		List<BootstrapPrologContribution> contribs = createCachedContribsForPrologInterface(contributionKey);

		String resource = element.getAttribute("path");
		String alias = element.getAttribute("alias");

		Set<String> dependencies = getContributionDependencies(element);

		addBootstrapResource(extension, resource, alias,contribs, dependencies, contributionId);
	}

	private Set<String> getContributionDependencies(IConfigurationElement element) {
		Set<String> dependencies = new HashSet<String>();
		for (IConfigurationElement childElm : element.getChildren()) {
			if ("dependency".equals(childElm.getName())) {
				if(childElm.getAttribute("contribution")==null) 
					System.out.println("DEBUG");
				dependencies.add(childElm.getAttribute("contribution"));
			}
		}
		return dependencies;
	}

	private List<BootstrapPrologContribution> createCachedContribsForPrologInterface(String contributionKey) {
		List<BootstrapPrologContribution> contribs = bootStrapContribForKey.get(contributionKey);
		if (contribs == null) {
			contribs = new ArrayList<BootstrapPrologContribution>();
			bootStrapContribForKey.put(contributionKey, contribs);
		}
		return contribs;
	}

	private void addBootstrapResource(IExtension ext, String resource, String alias, List<BootstrapPrologContribution> contribs,
			Set<String> dependencies, String contributionId) {
		Debug.debug("got this resname: " + resource);
		IContributor contributor = ext.getContributor();
		Debug.debug("got this contributor: " + contributor.toString());
		URL url = FileLocator.find(Platform.getBundle(contributor.getName()), new Path(resource), null);
		try {
			Debug.debug("trying to resolve this url: " + url);
			url = FileLocator.toFileURL(url);
		} catch (Exception e) {
			Debug.rethrow("Problem resolving path extension for bootstrapContribution, contributor: " + contributor.getName()
					+ " and resource: " + resource, e);
		}
		File file = new File(url.getFile());
		BootstrapPrologContribution contrib;
		if(alias != null){
			contrib = new BootstrapPrologContributionAlias(contributionId, alias, Util.prologFileName(file), dependencies);
		} else {
			contrib = new BootstrapPrologContributionFile(contributionId, Util.prologFileName(file), dependencies);
		}
		contribs.add(contrib);
		allBootStrapLists.put(contributionId, contrib);
	}


	private List<BootstrapPrologContribution> getBootstrapList(String contributionKey) {
		List<BootstrapPrologContribution> r = getBootStrapLists().get(contributionKey);
		return r == null ? new ArrayList<BootstrapPrologContribution>() : r;
	}

	protected void registerStaticHooks() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID, PrologRuntime.EP_HOOKS);
		if (point == null) {
			Debug.error("could not find the extension point " + PrologRuntime.EP_HOOKS);
			throw new RuntimeException("could not find the extension point " + PrologRuntime.EP_HOOKS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IExtension extension = extensions[i];
				IConfigurationElement[] celems = extension.getConfigurationElements();
				for (int j = 0; j < celems.length; j++) {

					final IConfigurationElement celem = celems[j];
					if (celem.getName().equals("registryHook")) {
						RegistryHook hook = (RegistryHook) celem.createExecutableExtension("class");
						registryHooks.add(hook);
					} else {
						Debug.warning("hmmm... asumed a hook, but got a " + celem.getName());
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}

	/**
	 * register all trackers that are defined using the extension point
	 * org.cs3.pdt.runtime.prologContextTracker.
	 * 
	 */
	protected void registerStaticTrackers() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID, PrologRuntime.EP_TRACKERS);
		if (point == null) {
			Debug.error("could not find the extension point " + PrologRuntime.EP_TRACKERS);
			throw new RuntimeException("could not find the extension point " + PrologRuntime.EP_TRACKERS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IConfigurationElement[] celem = extensions[i].getConfigurationElements();
				for (int j = 0; j < celem.length; j++) {

					if (!celem[j].getName().equals("tracker")) {
						Debug.warning("hmmm... asumed a tracker, but got a " + celem[j].getName());
					} else {
						AbstractPrologContextTracker tracker = (AbstractPrologContextTracker) celem[j].createExecutableExtension("class");

						String id = celem[j].getAttributeAsIs("id");
						String label = celem[j].getAttributeAsIs("label");
						tracker.setLabel(label);
						tracker.setId(id);
						getContextTrackerService().registerPrologContextTracker(tracker);
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}


	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		try {
			PrologInterfaceRegistry registry = getPrologInterfaceRegistry();
			Set<String> keys = new HashSet<String>(registry.getRegisteredKeys()); // clone this. see
			// PDT-194
			Iterator<String> it = keys.iterator();
			while ( it.hasNext()) {
				String key = it.next();
				PrologInterface pif = registry.getPrologInterface(key);
				try {
					pif.stop();
					registry.removePrologInterface(key);
				} catch (Throwable e) {
					Debug.warning("problems during shutdown of pif " + key);
					Debug.report(e);
				}

			}

		} finally {
			super.stop(context);
		}
	}

	public IPrologEventDispatcher getPrologEventDispatcher(PrologInterface pif) {
		IPrologEventDispatcher r = dispatchers.get(pif);
		if (r == null) {
			r = new UDPEventDispatcher(pif,getLibraryManager());
			dispatchers.put(pif, r);
		}
		return r;

	}

	/**
	 * get a PrologInterface to a given key. This is a convenience method to
	 * "just get the darn thing". This will create the PrologInterface if
	 * the registry does not contain it, and register it with the registry. No
	 * subscription will be added to the history.
	 * 
	 * @param key
	 * @return
	 * @throws PrologInterfaceException
	 */
	public PrologInterface getPrologInterface(String key) {
		DefaultSubscription defaultSubscription = new DefaultSubscription(null, key, null, null);
		return getPrologInterface(defaultSubscription);
	}

	/**
	 * Subscribe to a PrologInterface. If the registry does not contain a pif
	 * for key the subscription is for, this method will create a new pif and
	 * register it with the registry.
	 * 
	 * @param s
	 *            The subscription to use. If a subscription with the same id
	 *            exists, it is replaced. Must not be null.
	 * @return the ProlotInterface instance, either from registry, or freshly
	 *         created.
	 * @throws PrologInterfaceException
	 */
	public PrologInterface getPrologInterface(Subscription s) {
		PrologInterfaceRegistry r = getPrologInterfaceRegistry();
		String pifKey = s.getPifKey();
		PrologInterface pif = r.getPrologInterface(pifKey);
		if (pif == null) {
			pif = createPrologInterface(pifKey);
			initPrologInterfaceOptions(pif);
			r.addPrologInterface(pifKey, pif);
			addGlobalHooks(pifKey, pif);
		}
		List<String> contributionKeys = s.getBootstrapConstributionKeys();
		for (String contributionKey : contributionKeys) {
			List<BootstrapPrologContribution> libraryList = getBootstrapList(contributionKey);
			for (BootstrapPrologContribution library : libraryList) {
				if (!pif.getBootstrapLibraries().contains(library)) {
					pif.getBootstrapLibraries().add(library);
					if (pif.isUp()) {
						PrologSession session = null;
						try {
							session = pif.getSession(PrologInterface.LEGACY);
							
							String consult = library.getPrologInitStatement();
							Debug.debug("consult " + consult + ", from " + library);
							session.queryOnce(consult);
						} catch (PrologInterfaceException e) {
							Debug.report(e);
							if (session != null)
								session.dispose();
						}
					}
				}
			}
		}
		if (s.getId() != null) {
			r.addSubscription(s);
		}

		return pif;
	}

	private void addGlobalHooks(String pifKey, PrologInterface pif) {
		Map hooks = getGlobalHooks().get(pifKey);
		if (hooks != null) {
			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
				_HookRecord record = it.next();
				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
			}
		}
		hooks = getGlobalHooks().get("");
		if (hooks != null) {
			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
				_HookRecord record = it.next();
				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
			}
		}
	}

	public PrologInterfaceRegistry getPrologInterfaceRegistry() {
		synchronized (registryMux) {
			if (this.registry == null) {
				this.registry = new DefaultSAXPrologInterfaceRegistry();
				initRegistry();
			}
			return this.registry;
		}

	}

	private void initRegistry() {
		try {
			ISavedState lastState = ResourcesPlugin.getWorkspace().addSaveParticipant(this, new _SaveParticipant());

			if (lastState != null) {
				IPath location = lastState.lookup(new Path("registry"));
				location = getStateLocation().append(location);
				File file = location.toFile();
				if (file.canRead()) {
					Debug.info("Reading registry file " + file.getCanonicalPath());
					Reader r = new BufferedReader(new FileReader(file));
					registry.load(r);
				} else {
					Debug.warning("Registry file " + file.getCanonicalPath() + " could not be read. A new file will be created on exit.");
				}
			}
			registerStaticHooks();
			for (RegistryHook hook : registryHooks) {
				hook.addSubscriptions(registry);
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		} catch (IOException e) {
			Debug.rethrow(e);

		}
	}

	/**
	 * Checks if a PrologInterface is registered for the given key.
	 * 
	 * This method may be used by clients who want to check for the existence of
	 * a key in the registry without actualy creating a PrologInterface (yet).
	 * 
	 * This is aequivalent to calling
	 * <code>getPrologInterfaceRegistry().getRegisteredKeys().contains(pifKey)</code>
	 * 
	 * @param pifKey
	 * @return
	 */
	public boolean hasPrologInterface(String pifKey) {
		return getPrologInterfaceRegistry().getRegisteredKeys().contains(pifKey);
	}

	private final class _SaveParticipant implements ISaveParticipant {
		public void saving(ISaveContext context) throws CoreException {
			switch (context.getKind()) {
			case ISaveContext.FULL_SAVE:
				PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin.getDefault();
				// save the plug-in state
				int saveNumber = context.getSaveNumber();
				String saveFileName = "registry-" + Integer.toString(saveNumber);
				File f = myPluginInstance.getStateLocation().append(saveFileName).toFile();
				// if we fail to write, an exception is
				// thrown and we do not update the path
				Writer w = null;
				try {
					Debug.info("writing registry to " + f.getCanonicalPath());
					w = new BufferedWriter(new FileWriter(f));
					myPluginInstance.registry.save(w);
					w.close();
				} catch (IOException e) {
					Debug.rethrow(e);
				}
				context.map(new Path("registry"), new Path(saveFileName));
				context.needSaveNumber();
				break;
			case ISaveContext.PROJECT_SAVE:
				break;
			case ISaveContext.SNAPSHOT:
				break;
			}

		}

		public void rollback(ISaveContext context) {
			PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin.getDefault();

			// since the save operation has failed, delete
			// the saved state we have just written
			int saveNumber = context.getSaveNumber();
			String saveFileName = "registry-" + Integer.toString(saveNumber);
			File f = myPluginInstance.getStateLocation().append(saveFileName).toFile();
			f.delete();

		}

		public void prepareToSave(ISaveContext context) throws CoreException {
			;
		}

		public void doneSaving(ISaveContext context) {
			PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin.getDefault();

			// delete the old saved state since it is not
			// necessary anymore
			int previousSaveNumber = context.getPreviousSaveNumber();
			String oldFileName = "registry-" + Integer.toString(previousSaveNumber);
			File f = myPluginInstance.getStateLocation().append(oldFileName).toFile();
			f.delete();

		}
	}

	private static class _HookRecord {
		LifeCycleHook hook;

		String hookId;

		String[] deps;

		public _HookRecord(LifeCycleHook hook, String hookId, String[] deps) {
			super();
			this.hook = hook;
			this.hookId = hookId;
			this.deps = deps;
		}

	}




	private HashMap<String, Map> getGlobalHooks() {
		synchronized (globalHooksMux) {
			if (globalHooks == null) {
				globalHooks = new HashMap<String, Map>();
				registerStaticHooks();
			}
			return globalHooks;
		}
	}
	
	public String overridePreferenceBySystemProperty(String name) {
		String value;
		value = System.getProperty(name);

		if (value != null) {
			Debug.warning("option " + name + " is overridden by system property: " + value);
			return value;
		}
		
		value = getPreferenceStore().getString(name);
		
		return value;
	}
	
}
