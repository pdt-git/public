/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.ui;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.WeakHashMap;

import org.cs3.prolog.common.DefaultResourceFileLocator;
import org.cs3.prolog.common.ResourceFileLocator;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntime;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.Subscription;
import org.cs3.prolog.connector.internal.DefaultPrologContextTrackerService;
import org.cs3.prolog.internal.pif.service.PrologInterfaceService;
import org.cs3.prolog.lifecycle.IPrologEventDispatcher;
import org.cs3.prolog.lifecycle.UDPEventDispatcher;
import org.cs3.prolog.load.BootstrapPrologContribution;
import org.cs3.prolog.load.PrologLibraryManager;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.IPrologInterfaceService;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.EclipsePreferenceProvider;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

public class PrologRuntimeUIPlugin extends AbstractUIPlugin implements IStartup {
	private final static String PLUGIN_ID = "org.cs3.prolog.connector.ui";

	// The shared instance.
	private static PrologRuntimeUIPlugin plugin;
	// Resource bundle.
	private ResourceBundle resourceBundle;

	private DefaultResourceFileLocator resourceLocator;
	private PrologContextTrackerService contextTrackerService;
	private WeakHashMap<PrologInterface, IPrologEventDispatcher> dispatchers = new WeakHashMap<PrologInterface, IPrologEventDispatcher>();
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
		return plugin;
	}
	
	/**
	 * Returns the id of the PrologRuntimeUIPlugin
	 * @return id of the PrologRuntimeUIPlugin
	 */
	public static String getPluginId() {
		return PLUGIN_ID;
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
			resourceLocator = new DefaultResourceFileLocator(new File(System.getProperty("java.io.tmpdir")));			
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

	@Override
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
			@Override
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
		

		prologInterface = PrologRuntimePlugin.getDefault().newPrologInterface(name);
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

//	public void reconfigure() {
//		reconfigurePrologInterfaces();
//	}

//	private void reconfigurePrologInterfaces() {
//		PrologInterfaceRegistry r = getPrologInterfaceRegistry();
//		Set<String> keys = r.getRegisteredKeys();
//		for (Iterator<String> it = keys.iterator(); it.hasNext();) {
//			String key = it.next();
//			PrologInterface pif = r.getPrologInterface(key);
//			initPrologInterfaceOptions(pif);
//		}
//	}

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
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntimeUI.PLUGIN_ID, PrologRuntime.EP_TRACKERS);
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

						String id = celem[j].getAttribute("id");
						String label = celem[j].getAttribute("label");
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
	@Override
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
			r = new UDPEventDispatcher(pif);
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
		boolean addPifToRegistry = false;
		if (pif == null) {
			pif = createPrologInterface(pifKey);
			initPrologInterfaceOptions(pif);
			addPifToRegistry = true;
//			PrologRuntimePlugin.getDefault().addGlobalHooks(pifKey, pif);
		}
		List<String> contributionKeys = new ArrayList<String>();
		contributionKeys.addAll(s.getBootstrapConstributionKeys());
		if(!contributionKeys.contains("")){
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
		if (addPifToRegistry) {
			r.addPrologInterface(pifKey, pif);
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
	
	private IPrologInterfaceService prologInterfaceService;
	
	public IPrologInterfaceService getPrologInterfaceService() {
		if (prologInterfaceService == null) {
			prologInterfaceService = new PrologInterfaceService();
		}
		return prologInterfaceService;
	}
	
}


