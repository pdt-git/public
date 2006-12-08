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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.Reader;
import java.io.Writer;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import org.cs3.pdt.runtime.internal.DefaultPrologContextTrackerService;
import org.cs3.pdt.runtime.internal.DefaultSAXPrologInterfaceRegistry;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.DefaultPrologLibrary;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologLibrary;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.eclipse.core.resources.ISaveContext;
import org.eclipse.core.resources.ISaveParticipant;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.util.BundleUtility;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PrologRuntimePlugin extends AbstractUIPlugin implements IStartup {

	// The shared instance.
	private static PrologRuntimePlugin plugin;

	/**
	 * Returns the shared instance.
	 */
	public static PrologRuntimePlugin getDefault() {
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

	// Resource bundle.
	private ResourceBundle resourceBundle;

	private DefaultResourceFileLocator rootLocator;

	private Option[] options;

	private DefaultSAXPrologInterfaceRegistry registry;

	private PrologLibraryManager libraryManager;

	private PrologContextTrackerService contextTrackerService;

	private HashMap globalHooks;

	private Map bootStrapLists;

	private PrologInterfaceFactory factory;

	private final static Object contextTrackerMux = new Object();

	private final static Object libraryManagerMux = new Object();

	private final static Object globalHooksMux = new Object();

	private final static Object registryMux = new Object();

	private final static Object optionsMux = new Object();

	private static final Object preferencesMux = new Object();

	public PrologLibraryManager getLibraryManager() {
		synchronized (libraryManagerMux) {
			if (libraryManager == null) {

				libraryManager = new PrologLibraryManager();
				registerStaticLibraries();

			}

			return libraryManager;
		}
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
				PrologContextTracker[] contextTrackers = contextTrackerService
						.getContextTrackers();
				for (int i = 0; i < contextTrackers.length; i++) {
					contextTrackers[i].init(workbench);
				}
			}
		});

	}

	/**
	 * The constructor.
	 */
	public PrologRuntimePlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle
					.getBundle("prg.cs3.pdt.runtime.PrologRuntimePluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	public ResourceFileLocator getResourceLocator(String key) {
		if (rootLocator == null) {
			URL url = getDefault().getBundle().getEntry("/");
			File location = null;
			try {
				location = new File(Platform.asLocalURL(url).getFile());
			} catch (IOException t) {
				Debug.rethrow(t);
			}
			rootLocator = new DefaultResourceFileLocator(location);
		}
		return rootLocator.subLocator(key);
	}

	private PrologInterface createPrologInterface() {
		PrologInterface prologInterface = null;

		factory = getPrologInterfaceFactory();
		prologInterface = factory.create();

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

			IPreferencesService service = Platform.getPreferencesService();
			String qualifier = getBundle().getSymbolicName();
			String value = service
					.getString(qualifier, key, defaultValue, null);

			return System.getProperty(key, value);
		}
	}

	/**
	 * 
	 */
	public void reconfigure() {

		reconfigurePrologInterfaces();
	}

	private void reconfigurePrologInterfaces() {
		PrologInterfaceRegistry r = getPrologInterfaceRegistry();
		Set keys = r.getRegisteredKeys();
		for (Iterator it = keys.iterator(); it.hasNext();) {
			String key = (String) it.next();
			PrologInterface pif = r.getPrologInterface(key);
			reconfigurePrologInterface(key, pif);
		}
	}

	private void reconfigurePrologInterface(String key,
			PrologInterface prologInterface) {

		// MetadataEngineInstaller.install(prologInterface);
		List l = prologInterface.getBootstrapLibraries();
		l.addAll(getBootstrapList(""));
		l.addAll(getBootstrapList(key));

		// we are using the bootstrapContribution extension point just as
		// anybody else.
		// --lu
		// l.add(Util.prologFileName(getResourceLocator(PDT.LOC_ENGINE).resolve(
		// "main.pl")));
		PrologInterfaceFactory factory = prologInterface.getFactory();
		Option[] options = factory.getOptions();
		for (int i = 0; i < options.length; i++) {
			String id = options[i].getId();
			String val = getPreferenceValue(id, options[i].getDefault());
			try {
				prologInterface.setOption(id, val);
			} catch (Throwable t) {
				Debug.warning("could not set option " + options[i].getLabel()
						+ "(" + options[i].getId() + ")");
				Debug.report(t);
			}
		}

	}

	/**
	 * @deprecated Bootstrap Contributions stink.
	 * @return
	 */
	private Map getBootStrapLists() {
		if (bootStrapLists == null) {
			bootStrapLists = new HashMap();
			registerStaticBootstrapContributions();
		}
		return bootStrapLists;
	}

	private void registerStaticLibraries() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(
				PrologRuntime.PLUGIN_ID, PrologRuntime.EP_PROLOG_LIBRARY);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ PrologRuntime.EP_PROLOG_LIBRARY);
			throw new RuntimeException("could not find the extension point "
					+ PrologRuntime.EP_PROLOG_LIBRARY);
		}
		IExtension[] extensions = point.getExtensions();

		for (int i = 0; i < extensions.length; i++) {
			IExtension ext = extensions[i];
			IConfigurationElement[] configurationElements = ext
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement elm = configurationElements[j];

				String id = elm.getAttribute("id");
				String alias = elm.getAttribute("alias");
				String resName = elm.getAttribute("path");
				Debug.debug("got this resname: " + resName);
				String namespace = ext.getNamespace();
				Debug.debug("got this namespace: " + namespace);
				URL url = BundleUtility.find(Platform.getBundle(namespace),
						resName);
				try {

					// URL url = Platform.getBundle(namespace).getEntry(
					// resName);
					Debug.debug("trying to resolve this url: " + url);
					url = Platform.asLocalURL(url);
				} catch (Exception e) {
					Debug
							.rethrow(
									"Problem resolving url: " + url.toString(),
									e);
				}
				// URI uri = URI.create(url.toString());
				File file = new File(url.getFile());
				String path = Util.prologFileName(file);

				IConfigurationElement[] childElms = elm.getChildren();
				Map libAttrs = new HashMap();
				Set deps = new HashSet();
				for (int k = 0; k < childElms.length; k++) {
					IConfigurationElement childElm = childElms[k];
					if("dependency".equals(childElm.getName())){
						deps.add(childElm.getAttribute("library"));
					}
					else if("attribute".equals(childElm.getName())){
						libAttrs.put(childElm.getAttribute("id"), childElm.getAttribute("value"));
					}
					else {
						Debug.warning("within <library> element, i found an unexpected child element: "+childElm.getName());
					}
				}
				
				PrologLibrary lib = new DefaultPrologLibrary(id, deps, alias,
						path,libAttrs);
				getLibraryManager().addLibrary(lib);

			}
		}
	}

	private void registerStaticBootstrapContributions() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(
				PrologRuntime.PLUGIN_ID,
				PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
			throw new RuntimeException("could not find the extension point "
					+ PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
		}
		IExtension[] extensions = point.getExtensions();

		for (int i = 0; i < extensions.length; i++) {
			IExtension ext = extensions[i];
			IConfigurationElement[] configurationElements = ext
					.getConfigurationElements();
			for (int j = 0; j < configurationElements.length; j++) {
				IConfigurationElement elm = configurationElements[j];
				String pifKey = elm.getAttribute("key");
				if (pifKey == null) {
					pifKey = "";
				}
				Set contribs = (Set) bootStrapLists.get(pifKey);
				if (contribs == null) {
					contribs = new HashSet();
					bootStrapLists.put(pifKey, contribs);
				}
				String resName = elm.getAttribute("path");
				String className = elm.getAttribute("class");
				if (className != null) {
					BootstrapContribution bc = null;
					try {
						bc = (BootstrapContribution) elm
								.createExecutableExtension("class");
						bc.contributeToBootstrapList(pifKey, contribs);
					} catch (CoreException e1) {
						Debug.rethrow("Problem instantiating: "
								+ elm.getAttributeAsIs("class"), e1);
					}
				} else if (resName != null) {
					Debug.debug("got this resname: " + resName);
					String namespace = ext.getNamespace();
					Debug.debug("got this namespace: " + namespace);
					URL url = BundleUtility.find(Platform.getBundle(namespace),
							resName);
					try {

						// URL url = Platform.getBundle(namespace).getEntry(
						// resName);
						Debug.debug("trying to resolve this url: " + url);
						url = Platform.asLocalURL(url);
					} catch (Exception e) {
						Debug.rethrow("Problem resolving url: "
								+ url.toString(), e);
					}
					// URI uri = URI.create(url.toString());
					File file = new File(url.getFile());

					contribs.add(Util.prologFileName(file));
				}
			}
		}

	}

	/**
	 * @deprecated bootstrap contributions stink.
	 * @param key
	 * @return
	 */
	private Set getBootstrapList(String key) {
		Set r = (Set) getBootStrapLists().get(key);
		return r == null ? new HashSet() : r;
	}

	protected void registerStaticHooks() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(
				PrologRuntime.PLUGIN_ID, PrologRuntime.EP_HOOKS);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ PrologRuntime.EP_HOOKS);
			throw new RuntimeException("could not find the extension point "
					+ PrologRuntime.EP_HOOKS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IConfigurationElement[] celem = extensions[i]
						.getConfigurationElements();
				for (int j = 0; j < celem.length; j++) {

					if (!celem[j].getName().equals("hook")) {
						Debug.warning("hmmm... asumed a hook, but got a "
								+ celem[j].getName());
					} else {
						LifeCycleHook hook = (LifeCycleHook) celem[j]
								.createExecutableExtension("class");
						String dependsOn = celem[j]
								.getAttributeAsIs("dependsOn");
						if (dependsOn == null) {
							dependsOn = "";
						}
						String[] dependencies = Util.split(dependsOn, ",");
						String id = celem[j].getAttributeAsIs("id");
						String pifKey = celem[j].getAttributeAsIs("key");
						if (pifKey == null) {
							pifKey = "";
						}
						registerLifeCycleHook(pifKey, hook, id, dependencies);
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
		IExtensionPoint point = registry.getExtensionPoint(
				PrologRuntime.PLUGIN_ID, PrologRuntime.EP_TRACKERS);
		if (point == null) {
			Debug.error("could not find the extension point "
					+ PrologRuntime.EP_TRACKERS);
			throw new RuntimeException("could not find the extension point "
					+ PrologRuntime.EP_TRACKERS);
		}
		IExtension[] extensions = point.getExtensions();
		try {
			for (int i = 0; i < extensions.length; i++) {
				IConfigurationElement[] celem = extensions[i]
						.getConfigurationElements();
				for (int j = 0; j < celem.length; j++) {

					if (!celem[j].getName().equals("tracker")) {
						Debug.warning("hmmm... asumed a tracker, but got a "
								+ celem[j].getName());
					} else {
						AbstractPrologContextTracker tracker = (AbstractPrologContextTracker) celem[j]
								.createExecutableExtension("class");

						String id = celem[j].getAttributeAsIs("id");
						String label = celem[j].getAttributeAsIs("label");
						tracker.setLabel(label);
						tracker.setId(id);
						getContextTrackerService()
								.registerPrologContextTracker(tracker);
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}

	public Option[] getOptions() {
		synchronized (optionsMux) {
			if (options == null) {
				initOptions();
			}
			return this.options;
		}
	}

	/**
	 * 
	 */
	private void initOptions() {

		options = new Option[] {
				new SimpleOption(
						PrologRuntime.PREF_PIF_IMPLEMENTATION,
						"PrologInterfaceFactory implementation",
						"The factory to be used for creating PrologInterface instances",
						Option.STRING, PrologInterfaceFactory.DEFAULT),
				new SimpleOption(
						PrologRuntime.PREF_PIF_BOOTSTRAP_DIR,
						"PrologInterface Bootstrap Directory",
						"The PrologInterface needs to temporarily store some"
								+ "prolog files during bootstrapping. Any directory for which "
								+ "you have write permissions will do.",
						Option.DIR, System.getProperty("java.io.tmpdir")) };

	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		try {
			PrologInterfaceRegistry r = getPrologInterfaceRegistry();
			Set keys = new HashSet(r.getRegisteredKeys()); // clone this. see
															// PDT-194
			for (Iterator it = keys.iterator(); it.hasNext();) {
				String key = (String) it.next();
				PrologInterface pif = r.getPrologInterface(key);
				try {
					pif.stop();
					r.removePrologInterface(key);
				} catch (Throwable e) {
					Debug.warning("problems during shutdown of pif " + key);
					Debug.report(e);
				}

			}

		} finally {
			super.stop(context);
		}
	}

	/**
	 * get a PrologInterface to a given key. This is a convenience method to
	 * "just get the darn thing". This will use create the PrologInterface if
	 * the registry does not contain it, and register it with the registry. No
	 * subscription will be added to the history.
	 * 
	 * @param key
	 * @return
	 * @throws PrologInterfaceException
	 */
	public PrologInterface getPrologInterface(String key) {
		return getPrologInterface(new DefaultSubscription(null, key, null, null));
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
			pif = createPrologInterface();
			reconfigurePrologInterface(pifKey, pif);
			r.addPrologInterface(pifKey, pif);
			addGlobalHooks(pifKey, pif);
		}
		if (s.getId() != null) {
			r.addSubscription(s);
		}
		return pif;
	}

	private void addGlobalHooks(String pifKey, PrologInterface pif) {
		Map hooks = (Map) getGlobalHooks().get(pifKey);
		if (hooks != null) {
			for (Iterator it = hooks.values().iterator(); it.hasNext();) {
				_HookRecord record = (_HookRecord) it.next();
				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
			}
		}
		hooks = (Map) getGlobalHooks().get("");
		if (hooks != null) {
			for (Iterator it = hooks.values().iterator(); it.hasNext();) {
				_HookRecord record = (_HookRecord) it.next();
				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
			}
		}
		// pif.start();
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
			ISavedState lastState = ResourcesPlugin.getWorkspace()
					.addSaveParticipant(this, new _SaveParticipant());

			if (lastState != null) {
				IPath location = lastState.lookup(new Path("registry"));
				location = getStateLocation().append(location);
				File file = location.toFile();
				if (file.canRead()) {
					Debug.info("Reading registry file "
							+ file.getCanonicalPath());
					Reader r = new BufferedReader(new FileReader(file));
					registry.load(r);
				} else {
					Debug
							.warning("Regestry file "
									+ file.getCanonicalPath()
									+ " could not be read. A new file will be created on exit.");
				}
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
		return getPrologInterfaceRegistry().getRegisteredKeys()
				.contains(pifKey);
	}

	private final class _SaveParticipant implements ISaveParticipant {
		public void saving(ISaveContext context) throws CoreException {
			switch (context.getKind()) {
			case ISaveContext.FULL_SAVE:
				PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin
						.getDefault();
				// save the plug-in state
				int saveNumber = context.getSaveNumber();
				String saveFileName = "registry-"
						+ Integer.toString(saveNumber);
				File f = myPluginInstance.getStateLocation().append(
						saveFileName).toFile();
				// if we fail to write, an exception is
				// thrown and we do not update the path
				Writer w = null;
				try {
					Debug.info("writing regestry to " + f.getCanonicalPath());
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
			PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin
					.getDefault();

			// since the save operation has failed, delete
			// the saved state we have just written
			int saveNumber = context.getSaveNumber();
			String saveFileName = "registry-" + Integer.toString(saveNumber);
			File f = myPluginInstance.getStateLocation().append(saveFileName)
					.toFile();
			f.delete();

		}

		public void prepareToSave(ISaveContext context) throws CoreException {
			;
		}

		public void doneSaving(ISaveContext context) {
			PrologRuntimePlugin myPluginInstance = PrologRuntimePlugin
					.getDefault();

			// delete the old saved state since it is not
			// necessary anymore
			int previousSaveNumber = context.getPreviousSaveNumber();
			String oldFileName = "registry-"
					+ Integer.toString(previousSaveNumber);
			File f = myPluginInstance.getStateLocation().append(oldFileName)
					.toFile();
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

	/**
	 * globally register a hook with a given pifKey.
	 * 
	 * The plugin will store the key->hook association in a plugin-global table.
	 * If there is already a pif registered for the given key, the hook will be
	 * added emidiately. Otherwise it will be added the first time some client
	 * requests a prolog interface for the given key.
	 * 
	 * Note that no attempt is made to actualy execute the code in the hook if
	 * the pif is already running.
	 * 
	 * 
	 */
	private void registerLifeCycleHook(String pifKey, LifeCycleHook hook,
			String hookId, String[] deps) {
		synchronized (globalHooksMux) {
			Map hooks = (Map) getGlobalHooks().get(pifKey);

			if (hooks == null) {
				hooks = new HashMap();
				getGlobalHooks().put(pifKey, hooks);
			}
			hooks.put(hookId, new _HookRecord(hook, hookId, deps));
			PrologInterface pif = getPrologInterfaceRegistry()
					.getPrologInterface(pifKey);
			if (pif != null) {
				pif.addLifeCycleHook(hook, hookId, deps);
			}
		}
	}

	/**
	 * globally unregister a hook.
	 * 
	 * removes the hook from the plugin global table. If pif is registered with
	 * the given key, the hook is removed from this pif, too.
	 * 
	 * Note that no attempt is made to actualy execute the code in the hook if
	 * the pif is already running.
	 * 
	 * @param pifKey
	 * @param hook
	 */
	private void unregisterLifeCycleHook(String pifKey, String hookId) {
		synchronized (globalHooksMux) {
			Map hooks = (Map) getGlobalHooks().get(pifKey);

			if (hooks == null) {
				return;
			}
			hooks.remove(hookId);
			PrologInterface pif = getPrologInterfaceRegistry()
					.getPrologInterface(pifKey);
			if (pif != null) {
				pif.removeLifeCycleHook(hookId);
			}
		}
	}

	private HashMap getGlobalHooks() {
		synchronized (globalHooksMux) {
			if (globalHooks == null) {
				globalHooks = new HashMap();
				registerStaticHooks();
			}
			return globalHooks;
		}
	}

	public PrologInterfaceFactory getPrologInterfaceFactory() {
		if (factory == null) {

			String impl = getPreferenceValue(
					PrologRuntime.PREF_PIF_IMPLEMENTATION, null);
			String bootStrapDir = getPreferenceValue(
					PrologRuntime.PREF_PIF_BOOTSTRAP_DIR, System
							.getProperty("java.io.tmpdir"));
			if (impl == null) {
				throw new RuntimeException("The required property \""
						+ PrologRuntime.PREF_PIF_IMPLEMENTATION
						+ "\" was not specified.");
			}
			factory = PrologInterfaceFactory.newInstance(impl);
			factory.setResourceLocator(new DefaultResourceFileLocator(new File(
					bootStrapDir)));
			factory.setLibraryManager(getLibraryManager());
			
		}
		return factory;
	}

}
