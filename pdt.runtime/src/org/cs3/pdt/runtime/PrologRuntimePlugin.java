package org.cs3.pdt.runtime;

import java.io.File;
import java.io.IOException;
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
import org.cs3.pdt.runtime.internal.DefaultPrologInterfaceRegistry;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.IStartup;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
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

	private PrologInterfaceRegistry registry;

	private PrologLibraryManager libraryManager;

	private PrologContextTrackerService contextTrackerService;

	private HashMap globalHooks;

	private Map bootStrapLists;

	public PrologLibraryManager getLibraryManager() {
		if (libraryManager == null) {
			libraryManager = new PrologLibraryManager();
			registerStaticLibraries();
		}
		return libraryManager;
	}

	public PrologContextTrackerService getContextTrackerService() {
		if (contextTrackerService == null) {
			contextTrackerService = new DefaultPrologContextTrackerService();
			registerStaticTrackers();
		}
		return contextTrackerService;
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
				Debug.report(t);
				throw new RuntimeException(t);
			}

			rootLocator = new DefaultResourceFileLocator(location);
		}
		return rootLocator.subLocator(key);
	}

	/**
	 * @return the prolog interface instance shared among this plugin's
	 *         components.
	 * @throws IOException
	 */
	private PrologInterface createPrologInterface() {
		PrologInterface prologInterface = null;
		String impl = getPreferenceValue(PrologRuntime.PREF_PIF_IMPLEMENTATION,
				null);
		String bootStrapDir = getPreferenceValue(PrologRuntime.PREF_PIF_BOOTSTRAP_DIR,
				System.getProperty("java.io.tmpdir"));
		if (impl == null) {
			throw new RuntimeException("The required property \""
					+ PrologRuntime.PREF_PIF_IMPLEMENTATION
					+ "\" was not specified.");
		}
		PrologInterfaceFactory factory = PrologInterfaceFactory
				.newInstance(impl);
		factory.setResourceLocator(new DefaultResourceFileLocator(new File(bootStrapDir)));

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

		IPreferencesService service = Platform.getPreferencesService();
		String qualifier = getBundle().getSymbolicName();
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}

	/**
	 * 
	 */
	public void reconfigure() {
		PrologInterface pif = createPrologInterface();
		boolean restart = false;
		if (!pif.isDown()) {
			try {
				restart = true;
				pif.stop();
			} catch (IOException e1) {
				Debug.report(e1);
			}
		}
		try {
			// reconfigureDebugOutput();

			reconfigurePrologInterfaces();

			if (restart) {
				pif.start();
			}

		} catch (Throwable e) {
			Debug.report(e);
		}

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
				String pifKey = elm.getAttribute("key");
				if (pifKey == null) {
					pifKey = "";
				}
				Set contribs = (Set) bootStrapLists.get(pifKey);
				if (contribs == null) {
					contribs = new HashSet();
					bootStrapLists.put(pifKey, contribs);
				}
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
					Debug.report(e);
					throw new RuntimeException("Problem resolving url: "
							+ url.toString(), e);
				}
				// URI uri = URI.create(url.toString());
				File file = new File(url.getFile());
				String path = Util.prologFileName(file);
				
				IConfigurationElement[] depElms = elm.getChildren();
				Set deps = new HashSet();
				for (int k = 0; k < depElms.length; k++) {
					IConfigurationElement depElm = depElms[k];
					deps.add(depElm.getAttribute("library"));
				}
				PrologLibrary lib = new DefaultPrologLibrary(id,deps,alias,path);
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
						Debug.report(e1);
						throw new RuntimeException("Problem instantiating: "
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
						Debug.report(e);
						throw new RuntimeException("Problem resolving url: "
								+ url.toString(), e);
					}
					// URI uri = URI.create(url.toString());
					File file = new File(url.getFile());

					contribs.add(Util.prologFileName(file));
				}
			}
		}

	}

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
						String[] dependencies = dependsOn.split(",");
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
			Debug.report(e);
			throw new RuntimeException(e);
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
			Debug.report(e);
			throw new RuntimeException(e);
		}

	}

	public Option[] getOptions() {
		if (options == null) {
			initOptions();
		}
		return this.options;
	}

	/**
	 * 
	 */
	private void initOptions() {

		options = new Option[] { new SimpleOption(
				PrologRuntime.PREF_PIF_IMPLEMENTATION,
				"PrologInterfaceFactory implementation",
				"The factory to be used for creating PrologInterface instances",
				Option.STRING, PrologInterfaceFactory.DEFAULT),
				new SimpleOption(
						PrologRuntime.PREF_PIF_BOOTSTRAP_DIR,
						"PrologInterface Bootstrap Directory",
						"The PrologInterface needs to temporarily store some" +
						"prolog files during bootstrapping. Any directory for which " +
						"you have write permissions will do.",
						Option.DIR, System.getProperty("java.io.tmpdir"))};

	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		try {
			PrologInterfaceRegistry r = getPrologInterfaceRegistry();
			Set keys = r.getRegisteredKeys();
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

	public PrologInterface getPrologInterface(String key) {
		return getPrologInterface(new PrologInterfaceSubscription(key,
				"No description available."), null);
	}

	public PrologInterface getPrologInterface(String key, String defaultName) {
		return getPrologInterface(new PrologInterfaceSubscription(key,
				"No description available."), defaultName);
	}

	public PrologInterface getPrologInterface(PrologInterfaceSubscription s) {
		return getPrologInterface(s, null);
	}

	/**
	 * retrieve a PrologInterface instance identified by the given key. If the
	 * registry does not contain a pif for that key, it will create a new pif
	 * and register it with the registry.
	 * 
	 * @param key
	 *            A unique identifier for the PrologInterface. Must NOT be null.
	 * @param s
	 *            Information describing what use the caller is about to make of
	 *            the pif. This is used by the ui to help the user to
	 *            distinguish multiple active instances. Can be null, in which
	 *            case some default information will be generated.
	 * @param defaultName
	 *            A default name for the pif. If the no pif existed for the
	 *            given key, or if no name was attached to it until now, the
	 *            given string will be used. May be null.
	 * @return the ProlotInterface instance, either from registry, or freshly
	 *         created.
	 */
	public PrologInterface getPrologInterface(PrologInterfaceSubscription s,
			String defaultName) {
		PrologInterfaceRegistry r = getPrologInterfaceRegistry();
		PrologInterface pif = r.getPrologInterface(s.key);
		if (pif == null) {
			pif = createPrologInterface();
			reconfigurePrologInterface(s.key, pif);
			r.addPrologInterface(s.key, pif);
			Map hooks = (Map) getGlobalHooks().get(s.key);
			if (hooks != null) {
				for (Iterator it = hooks.values().iterator(); it.hasNext();) {
					_HookRecord record = (_HookRecord) it.next();
					pif.addLifeCycleHook(record.hook, record.hookId,
							record.deps);
				}
			}
			hooks = (Map) getGlobalHooks().get("");
			if (hooks != null) {
				for (Iterator it = hooks.values().iterator(); it.hasNext();) {
					_HookRecord record = (_HookRecord) it.next();
					pif.addLifeCycleHook(record.hook, record.hookId,
							record.deps);
				}
			}
			try {
				pif.start();

			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
		if (r.getName(s.key) == null && defaultName != null) {
			r.setName(s.key, defaultName);
		}

		r.addSubscription(s);
		return pif;
	}

	public PrologInterfaceRegistry getPrologInterfaceRegistry() {
		if (this.registry == null) {
			this.registry = new DefaultPrologInterfaceRegistry();
		}
		return this.registry;

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

	private static class _HookRecord {
		LifeCycleHook hook;

		String hookId;

		String[] deps;

		public _HookRecord(LifeCycleHook hook, String hookId, String[] deps) {
			super();
			// TODO Auto-generated constructor stub
			this.hook = hook;
			this.hookId = hookId;
			this.deps = deps;
		}

	}

	/**
	 * globally register a hook with a given pifKey.
	 * 
	 * clients may use this method make sure there code at the earliest possible
	 * point. The plugin in will store the key->hook association in a
	 * plugin-global table. If there is already a pif registered for the given
	 * key, the hook will be added emidiately. Otherwise it will be added the
	 * first time some client requests a prolog interface for the given key.
	 * 
	 * Note that no attempt is made to actualy execute the code in the hook if
	 * the pif is already running.
	 * 
	 * Further note that the plugin global table consideres two entries equal,
	 * if and only if their hookIds are equal.
	 * 
	 * @param pifKey
	 * @param hook
	 */
	public void registerLifeCycleHook(String pifKey, LifeCycleHook hook,
			String hookId, String[] deps) {
		Map hooks = (Map) getGlobalHooks().get(pifKey);
		if (hooks == null) {
			hooks = new HashMap();
			getGlobalHooks().put(pifKey, hooks);
		}
		hooks.put(hookId, new _HookRecord(hook, hookId, deps));
		PrologInterface pif = getPrologInterfaceRegistry().getPrologInterface(
				pifKey);
		if (pif != null) {
			pif.addLifeCycleHook(hook, hookId, deps);
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
	public void unregisterLifeCycleHook(String pifKey, String hookId) {
		Map hooks = (Map) getGlobalHooks().get(pifKey);
		if (hooks == null) {
			return;
		}
		hooks.remove(hookId);
		PrologInterface pif = getPrologInterfaceRegistry().getPrologInterface(
				pifKey);
		if (pif != null) {
			pif.removeLifeCycleHook(hookId);
		}
	}

	private HashMap getGlobalHooks() {
		if (globalHooks == null) {
			globalHooks = new HashMap();
			registerStaticHooks();
		}
		return globalHooks;
	}

}
