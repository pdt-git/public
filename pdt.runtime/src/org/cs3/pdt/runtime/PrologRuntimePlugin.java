package org.cs3.pdt.runtime;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.DefaultPrologLibrary;
import org.cs3.pl.prolog.PrologLibrary;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IContributor;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class PrologRuntimePlugin extends Plugin {

	// The shared instance.
	private static PrologRuntimePlugin plugin;

	private Map<String, BootstrapPrologContribution> allBootStrapLists = new HashMap<String, BootstrapPrologContribution>();
	private Map<String, List<BootstrapPrologContribution>> bootStrapContribForKey;
//	private HashMap<String, Map> globalHooks;
	private PrologInterfaceRegistry registry;


	private static PrologLibraryManager libraryManager;
//	private final static Object globalHooksMux = new Object();
	private final static Object libraryManagerMux = new Object();
	private final static Object registryMux = new Object();

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
	}
	
	
	/**
	 * Returns the shared instance.
	 */
	public static PrologRuntimePlugin getDefault() {
		if (plugin == null)
			plugin = new PrologRuntimePlugin();
		return plugin;
	}

	private Map<String, List<BootstrapPrologContribution>> getBootStrapLists() {
		if (bootStrapContribForKey == null) {
			bootStrapContribForKey = new HashMap<String, List<BootstrapPrologContribution>>();
			registerStaticBootstrapContributions();
		}
		return bootStrapContribForKey;
	}

	private void registerStaticBootstrapContributions() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID, PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);

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
				Debug.report(e);
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


	public List<BootstrapPrologContribution> getBootstrapList(String contributionKey) {
		List<BootstrapPrologContribution> r = getBootStrapLists().get(contributionKey);
		return r == null ? new ArrayList<BootstrapPrologContribution>() : r;
	}

	/*********
	 * Prolog Library related
	 *********/
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
	
	private static void registerStaticLibraries() {
		IExtensionRegistry registry = Platform.getExtensionRegistry();
		IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID, PrologRuntime.EP_PROLOG_LIBRARY);
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

				URL url = FileLocator.find(Platform.getBundle(namespace), new Path(resName),null);
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


//	public void addGlobalHooks(String pifKey, PrologInterface pif) {
//		Map hooks = getGlobalHooks().get(pifKey);
//		if (hooks != null) {
//			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
//				_HookRecord record = it.next();
//				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
//			}
//		}
//		hooks = getGlobalHooks().get("");
//		if (hooks != null) {
//			for (Iterator<_HookRecord> it = hooks.values().iterator(); it.hasNext();) {
//				_HookRecord record = it.next();
//				pif.addLifeCycleHook(record.hook, record.hookId, record.deps);
//			}
//		}
//	}

	/**
	 *  
	 * @return prolog registry
	 */
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
		registerStaticHooks();
	}
	

//	private HashMap<String, Map> getGlobalHooks() {
//		synchronized (globalHooksMux) {
//			if (globalHooks == null) {
//				globalHooks = new HashMap<String, Map>();
//				registerStaticHooks();
//			}
//			return globalHooks;
//		}
//	}
	
//	private static class _HookRecord {
//		LifeCycleHook hook;
//
//		String hookId;
//
//		String[] deps;
//
//		public _HookRecord(LifeCycleHook hook, String hookId, String[] deps) {
//			super();
//			this.hook = hook;
//			this.hookId = hookId;
//			this.deps = deps;
//		}
//
//	}
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
						hook.addSubscriptions(this.registry);
					} else {
						Debug.warning("hmmm... asumed a hook, but got a " + celem.getName());
					}
				}
			}
		} catch (CoreException e) {
			Debug.rethrow(e);
		}

	}


}
