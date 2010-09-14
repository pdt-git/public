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

package org.cs3.pdt.runtime.ui;

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
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.WeakHashMap;

import org.cs3.pdt.runtime.BootstrapPrologContribution;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.RegistryHook;
import org.cs3.pdt.runtime.Subscription;
import org.cs3.pdt.runtime.internal.DefaultPrologContextTrackerService;
import org.cs3.pdt.ui.util.EclipsePreferenceProvider;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.PreferenceProvider;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.UDPEventDispatcher;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
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
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PrologRuntimeUIPlugin extends AbstractUIPlugin implements IStartup {


	// The shared instance.
	private static PrologRuntimeUIPlugin plugin;
	// Resource bundle.
	private ResourceBundle resourceBundle;

	private DefaultResourceFileLocator resourceLocator;
	private PrologContextTrackerService contextTrackerService;
	private WeakHashMap<PrologInterface, IPrologEventDispatcher> dispatchers = new WeakHashMap<PrologInterface, IPrologEventDispatcher>();
	private HashSet<RegistryHook> registryHooks = new HashSet<RegistryHook>();

	private final static Object contextTrackerMux = new Object();
	private static final Object preferencesMux = new Object();

	public PrologRuntimeUIPlugin() {
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
	public static PrologRuntimeUIPlugin getDefault() {
		if (plugin == null)
			plugin = new PrologRuntimeUIPlugin();
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



	public ResourceFileLocator getResourceLocator() {
		if (resourceLocator == null){
			String bootStrapDir = getPreferenceValue(PrologRuntimeUI.PREF_PIF_BOOTSTRAP_DIR, System.getProperty("java.io.tmpdir"));
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
		prologInterface.initOptions(new EclipsePreferenceProvider(this));
		
		prologInterface.setFileSearchPath(PrologRuntimePlugin.guessFileSearchPath("pdt.runtime.socket.codebase"));
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

	private PrologInterfaceRegistry getPrologInterfaceRegistry() {
		return PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
	}

	private void initPrologInterfaceOptions(PrologInterface prologInterface) {
		prologInterface.initOptions(new EclipsePreferenceProvider(this));
	}





	/**
	 * register all trackers that are defined using the extension point
	 * org.cs3.pdt.runtime.prologContextTracker.
	 * 
	 */
	protected void registerStaticTrackers() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntimeUI.PLUGIN_ID, PrologRuntimeUI.EP_TRACKERS);
		if (point == null) {
			Debug.error("could not find the extension point " + PrologRuntimeUI.EP_TRACKERS);
			throw new RuntimeException("could not find the extension point " + PrologRuntimeUI.EP_TRACKERS);
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
			r = new UDPEventDispatcher(pif,PrologRuntimePlugin.getLibraryManager());
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
			PrologRuntimePlugin.getDefault().addGlobalHooks(pifKey, pif);
		}
		List<String> contributionKeys = s.getBootstrapConstributionKeys();
		if(contributionKeys.size() == 0){
			contributionKeys.add("");
		}
		for (String contributionKey : contributionKeys) {
			List<BootstrapPrologContribution> libraryList = PrologRuntimePlugin.getDefault().getBootstrapList(contributionKey);
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

	


	/**
	 * Checks if a PrologInterface is registered for the given key.
	 * 
	 * This method may be used by clients who want to check for the existence of
	 * a key in the registry without actually creating a PrologInterface (yet).
	 * 
	 * This is equivalent to calling
	 * <code>getPrologInterfaceRegistry().getRegisteredKeys().contains(pifKey)</code>
	 * 
	 * @param pifKey
	 * @return
	 */
	public boolean hasPrologInterface(String pifKey) {
		return getPrologInterfaceRegistry().getRegisteredKeys().contains(pifKey);
	}



	public PrologLibraryManager getLibraryManager() {
		return PrologRuntimePlugin.getLibraryManager();
	}
	
}
