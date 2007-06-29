/*
 */
package org.cs3.jtransformer;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.Vector;

import org.cs3.jtransformer.internal.natures.JTransformerNature;
import org.cs3.jtransformer.internal.natures.JTransformerSubscription;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.DefaultErrorMessageProvider;
import org.cs3.pdt.ui.util.ErrorMessageProvider;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.OptionProviderEvent;
import org.cs3.pl.common.OptionProviderListener;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceChangeEvent;
import org.eclipse.core.resources.ISavedState;
import org.eclipse.core.resources.IWorkspaceRunnable;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Preferences;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The Java Logical Meta-Programming.(aka JTransformer) Plugin. <br>
 * Not much here. You may want to see JTransformerProject.
 * 
 * @see JTransformerProject
 */
public class JTransformerPlugin extends AbstractUIPlugin {

    private static JTransformerPlugin plugin;

	private static Map natures = new Hashtable();

    private ResourceBundle resourceBundle;

    private ResourceFileLocator rootLocator;

    private Vector projectlisteners = new Vector();

  
    private Option[] options;

    private Hashtable listenerSet = new Hashtable();

    public JTransformerPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("prg.cs3.pdt.PDTPluginResources"); //$NON-NLS-1$
        } catch (MissingResourceException x) {
            resourceBundle = null;
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
     */
    public void start(BundleContext context) throws Exception {
        super.start(context);
        initOptions();
        reconfigureDebugOutput();
        getPluginPreferences();
        collectListeners();
        ResourcesPlugin.getWorkspace().addResourceChangeListener(
        		new JTransformerProjectChangeListener(),
        		IResourceChangeEvent.PRE_CLOSE | IResourceChangeEvent.PRE_DELETE |
        		IResourceChangeEvent.POST_CHANGE);
        PrologInterfaceRegistry pir = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
        JTDebug.debug("Prolog Interface keys found: " + pir.getAllKeys());
		ISavedState lastState = ResourcesPlugin.getWorkspace()
		.addSaveParticipant(this, new JTransformerSaveParticipant());

    }

    /**
     * @throws CoreException
     *  
     */
    private void initOptions() throws CoreException {
        String storeFile = getResourceLocator("").resolve("global_pef_store.pl").toString(); //$NON-NLS-1$ //$NON-NLS-2$
		String fileSep = File.separator;
		String location = "";
		try {
			location = getLocation();
		} catch (IOException e) {
			JTDebug.report(e);
			JTDebug.error("Could not find plugin installation dir.");
		}
        
        options = new Option[] { 
//                new SimpleOption(
//                JTransformer.PREF_DEFAULT_OUTPUT_PROJECT,
//                "Default output source folder", //$NON-NLS-1$
//                "Used as default value for JTransformer Projects that do not specify their own output folder.", //$NON-NLS-1$
//                Option.STRING, src),
//                new SimpleOption(
//                JTransformer.PREF_DEFAULT_OUTPUT_FOLDER,
//                "Default output source folder", //$NON-NLS-1$
//                "Used as default value for JTransformer Projects that do not specify their own output folder.", //$NON-NLS-1$
//                Option.STRING, src),
				new SimpleOption(JTransformer.PREF_DEBUG_LEVEL, "Debug Level",
						"Determines the verbosity of the debug log file.",
						Option.ENUM, "WARNING", new String[][] {
								{ "error", "ERROR" }, { "warning", "WARNING" },
								{ "info", "INFO" }, { "debug", "DEBUG" } }),
				new SimpleOption(
						JTransformer.PREF_CLIENT_LOG_FILE,
						"Log file location",
						"A file to which debug output of JTransformer will be writen",
						Option.FILE, location + fileSep + "jtransformer.log"),
        		
                new SimpleOption(
                        JTransformer.PREF_USE_PEF_STORE,
                        "Use PEF store (EXPERIMENTAL)", //$NON-NLS-1$
                        "If enabled, JTransformer will save PEFs before shutdown and reload them at startup. ", //$NON-NLS-1$
                        Option.FLAG, "false"), //$NON-NLS-1$
                new SimpleOption(
                        JTransformer.PREF_DEFAULT_PEF_STORE_FILE,
                        "Default PEF store file", //$NON-NLS-1$
                        "Used as default value for JTransformer Projects " //$NON-NLS-1$
                                + "that do not specify their own store file.", //$NON-NLS-1$
                        Option.FILE, storeFile),
                new SimpleOption(
                        JTransformer.PREF_REVERSE_INDEX,
                        "Create Reverse Index", //$NON-NLS-1$
                        "Reverse indexes are automatically created for every PEF.", //$NON-NLS-1$
                        Option.FLAG, "false")
                };

    }

	private String getLocation() throws IOException {
		URL url = JTransformerPlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		// replaced Platform.asLocalURL(url) with FileLocator.toFileURL(url) 
		location = new File(FileLocator.toFileURL(url).getFile())
				.getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}

    
//    public IClasspathEntry getFirstSourceFolder(IJavaProject javaProject) throws JavaModelException {
//        IClasspathEntry[] cp = javaProject.getResolvedClasspath(true);
//        for(int i=0;i<cp.length;i++){
//            if(cp[i].getEntryKind()==IClasspathEntry.CPE_SOURCE){
//               return cp[i];
//            }
//        }
//        return null;
//    }

//    /**
//     * @return
//     * @throws CoreException
//     */
//    private IJavaProject getDummyOutput() throws CoreException {
//        String projectName = "test-output"; //$NON-NLS-1$
//        IProject project = ResourcesPlugin.getWorkspace().getRoot().getProject(projectName);
//        if(!project.exists()){
//            project=createProject(projectName);
//        }
//        if(!project.isOpen()){
//            project.open(null);
//        }
//        IJavaProject javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
//        if(!project.hasNature(JavaCore.NATURE_ID)){            
//            addNature(project, JavaCore.NATURE_ID);            
//            IClasspathEntry[] cp = new IClasspathEntry[] {
//                    JavaCore.newSourceEntry(project.getFullPath()),
//                    JavaRuntime.getDefaultJREContainerEntry(),
//
//            };
//            javaProject = (IJavaProject) project.getNature(JavaCore.NATURE_ID);
//            javaProject.setRawClasspath(cp, project.getFullPath(),
//                    null);
//        }
//        return javaProject;
//    }
    /*
     * Create simple project.
     */
    private IProject createProject(final String projectName)
            throws CoreException {
        final IProject project = ResourcesPlugin.getWorkspace().getRoot()
                .getProject(projectName);
        IWorkspaceRunnable create = new IWorkspaceRunnable() {
            public void run(IProgressMonitor monitor) throws CoreException {
                if (project.exists()) {
                    try {
                        project.open(null);
                    } catch (Throwable t) {

                    }
                    project.delete(true, null);
                }
                project.create(null);
                project.open(null);
            }
        };
        ResourcesPlugin.getWorkspace().run(create,null);
        return project;
    }
    
    private void addNature(IProject project, String id) throws CoreException {
        if (!project.hasNature(id)) {
            IProjectDescription ipd = project.getDescription();
            String[] oldNIDs = ipd.getNatureIds();
            String[] newNIDs = new String[oldNIDs.length + 1];
            newNIDs[0] = id;
            System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
            ipd.setNatureIds(newNIDs);
            if (!project.isSynchronized(IResource.DEPTH_ONE)) {
                project.refreshLocal(IResource.DEPTH_ONE, null);
            }
            project.setDescription(ipd, null);
        }
    }
    /**
     * Returns the shared instance.
     */
    public static JTransformerPlugin getDefault() {
        return plugin;
    }

    /**
     * Returns a resource file locator for a given key.
     * <p>
     * The current implementation returns the value of
     * <code>rootLocator.subLocator(key)</code> where root locator "points" to
     * the installation directory of the plugin.
     * 
     * @param key
     *                should be a valid filesystem path element.
     * @return a resource file locator
     */
    public ResourceFileLocator getResourceLocator(String key) {
        if (rootLocator == null) {
            URL url = getBundle().getEntry("/"); //$NON-NLS-1$
            File location = null;
            try {
                location = new File(Platform.asLocalURL(url).getFile());
            } catch (IOException t) {
                JTDebug.report(t);
                throw new RuntimeException(t);
            }

            rootLocator = new DefaultResourceFileLocator(location);
        }
        return rootLocator.subLocator(key);
    }

    private void collectListeners() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint("org.cs3.jtransformer", //$NON-NLS-1$
                JTransformer.EP_PROJECT_LISTENER);
        if (point == null) {
            JTDebug.error("could not find the extension point " //$NON-NLS-1$
                    + JTransformer.EP_PROJECT_LISTENER);
            return;
        }
        IExtension[] extensions = point.getExtensions();
        try {
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] celem = extensions[i]
                        .getConfigurationElements();
                for (int j = 0; j < celem.length; j++) {

                    if (!celem[j].getName().equals("listener")) { //$NON-NLS-1$
                        JTDebug.warning("hmmm... asumed a listener, but got a " //$NON-NLS-1$
                                + celem[j].getName());
                    } else {
                        JTransformerProjectListener listener = (JTransformerProjectListener) celem[j]
                                .createExecutableExtension("class"); //$NON-NLS-1$
                        projectlisteners.add(listener);
                    }
                }
            }
        } catch (CoreException e) {
            JTDebug.report(e);
        }
    }

    /**
     * notify lregistered listeners. <br>
     * This method is typicaly called by a JTransformerProject when it recieves
     * notification from the builder. This method multiplexes the call to
     * listeners registered via the JTransformer Project Listener extension point <br>
     * Clients should not call this method directly.
     * 
     * @param e
     *                the update event.
     */
    public void fireFactBaseUpdated(JTransformerProjectEvent e) {
        Vector cloned = null;
        synchronized (projectlisteners) {
            cloned = (Vector) projectlisteners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            JTransformerProjectListener l = (JTransformerProjectListener) it.next();
            l.factBaseUpdated(e);
        }
    }

    public Option[] getOptions() {
        return this.options;
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

        // XXX: schmatz: is the try-catch ok? returning default value ok?
        
        // schmatz: surrounded with try-catch
        String value = null;
        try
		{
            // XXX: schmatz: Note: the next line has thrown a NPE everytime when closing the runtime Eclipse IDE!
        	value = service.getString(qualifier, key, defaultValue, null);	
		}
        catch (Exception e)
		{
			return defaultValue;
		}
        
        return System.getProperty(key, value);
    }
    
    /**
     * conveniance method. should propably be inlined. --lu
     */
    public void setPreferenceValue(String key, String value){
        Preferences prefStore = getPluginPreferences();
        prefStore.setValue(key, value);
        savePluginPreferences();
    }

    /**
     * @throws CoreException 
     */
    public void setPreferenceValue(IProject project, String key, String value) throws CoreException{
		project.setPersistentProperty(new QualifiedName("", key), value);
   		fireValueChanged(project,key);
    }    
    
	public String getPreferenceValue(IProject project, String key, String defaultValue) {
		try {
			String value = project.getPersistentProperty(new QualifiedName("", key));
			if(value != null) {
				return value;
			}
			return defaultValue;
		} catch (CoreException e) {
			JTDebug.report(e);
			throw new RuntimeException(e);
		}
	}
    
    /**
     * Only used for setting nonpersistant state for the JTransformer icon decorations.
     * 
     * conveniance method. should propably be inlined. --lu
     * @throws CoreException 
     */
    public void setNonPersistentPreferenceValue(IProject project, String key, String value) throws CoreException{
    	Map projectPref = getProjectPreference(project);
    	projectPref.put(key,value);
   		fireValueChanged(project,key);
    }    
    
    Map nonPersistentPreference = new Hashtable();

//	private Map tempSubscription = new Hashtable();
    
	/**
	 * Only used for getting nonpersistant state for the JTransformer icon decorations.
     * 
	 */
    public String getNonPersistentPreferenceValue(IProject project, String key, String defaultValue) {
    	Map projectPref = getProjectPreference(project);
		String value = (String)projectPref.get(key);
		if(value != null) {
			return value;
		}
		return defaultValue;
	}

    private Map getProjectPreference(IProject project) {
    	synchronized (nonPersistentPreference) {
			Map projectPref = (Map)nonPersistentPreference.get(project);
			if(projectPref == null) {
				projectPref = new HashMap();
				nonPersistentPreference.put(project,projectPref);
			}
			return projectPref;
		}

	}

	/**
     * reload preferences and configure components.
     * <br><b>NOTE:</b> clients should not call this method directly.
     */
    public void reconfigure() {
        try {
//			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
//					.getProjects();
//			for (int i = 0; i < projects.length; i++) {
//				IProject project = projects[i];
//
//				if (project.isAccessible() && project.hasNature(JTransformer.NATURE_ID)) {
//					getNature(project).reconfigure();
//					
//				}
//			}
			reconfigureDebugOutput();

		} catch (Throwable e) {
			JTDebug.report(e);
			throw new RuntimeException(e);
		}
    }
    /**
     * @throws PrologInterfaceException 
     * @throws PrologException 
     * @deprecated 
     */
    public  void reload(PrologSession initSession) throws PrologException, PrologInterfaceException {       
        String storeName = JTransformerPlugin.getDefault().getPreferenceValue(JTransformer.PREF_DEFAULT_PEF_STORE_FILE,null);
        File storeFile = new File(storeName);
         if(storeFile.canRead()){
            initSession.queryOnce("['"+Util.prologFileName(storeFile)+"']"); //$NON-NLS-1$ //$NON-NLS-2$
        }
    }
    /**
     * @throws PrologInterfaceException 
     * @throws PrologException 
     * @deprecated 
     */    
    public  void save(PrologSession shutdownSession) throws PrologException, PrologInterfaceException{
        String storeName = JTransformerPlugin.getDefault().getPreferenceValue(JTransformer.PREF_DEFAULT_PEF_STORE_FILE,null);
        File storeFile = new File(storeName);
        
            shutdownSession.queryOnce(JTPrologFacade.WRITE_TREE_FACTS +"('"+Util.prologFileName(storeFile)+"')"); //$NON-NLS-1$ //$NON-NLS-2$
        
    }

	public ErrorMessageProvider getErrorMessageProvider()
	{
		return new JTransformerErrorMessageProvider(plugin);
	}
	
	public void createPrologInterfaceExceptionCoreExceptionWrapper(PrologInterfaceException e1) throws CoreException
	{
		throw new CoreException(UIUtils.createErrorStatus(
				new DefaultErrorMessageProvider(getDefault()), e1,
				JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION));
	}

	class JTransformerErrorMessageProvider implements ErrorMessageProvider {
		
		private Plugin plugin;

		public JTransformerErrorMessageProvider(Plugin plugin) {
			this.plugin = plugin;
		}

		public String getErrorMessage(int errCode)
		{
			return Messages.getString("JTransformerPlugin." + errCode) + 
			"Please inform the JTransformer developers and include the stack trace below."; //$NON-NLS-1$
		}

		public String getContextMessage(int cxCode)
		{
			return Messages.getString("JTransformerPlugin." + cxCode); //$NON-NLS-1$
		}

		public String getId()
		{
			return getPlugin().getBundle().getSymbolicName();
		}

		public Plugin getPlugin()
		{
			return plugin;
		}
		
	}

	public void addOptionProviderListener(IProject project, OptionProviderListener listener) {
		synchronized (listenerSet) {
			Set set = getListenerSet(project);
			set.add(listener);	
		}
	}

	public void removeOptionProviderListener(IProject project, OptionProviderListener listener) {
		synchronized (listenerSet) {
			Set set = getListenerSet(project);
			set.remove(listener);	
		}

	}
	private Set getListenerSet(IProject project) {
		Set set = (Set)listenerSet.get(project);
		if(set == null) {
			set = new HashSet();
			listenerSet.put(project, set);
		}
		return set;
	}

	public void fireValueChanged(IProject project, String id) {
		Set clone = new HashSet();
		OptionProviderEvent e = new OptionProviderEvent(this, id);

		synchronized (listenerSet) {
			Set set = getListenerSet(project);
			clone.addAll(set);
		}
		for (Iterator it = clone.iterator(); it.hasNext();) {
			OptionProviderListener l = (OptionProviderListener) it.next();
			l.valuesChanged(e);
		}
		
	}

	/**
	 * Do not use project.getNature(JTransformer.NATURE_ID) anywhere
	 * in you project. This entry point is important to avoid
	 * the creation of more than one JT nature for a project.
	 * 
	 * This method is synchronized on the IProject instance.
	 * 
	 * 
	 * @param project
	 * @return
	 * @throws CoreException
	 */
	static public JTransformerNature getNature(IProject project) throws CoreException {
		
		synchronized (JTransformerPlugin.class) {
			JTransformerNature nature = (JTransformerNature)project.getNature(JTransformer.NATURE_ID);
			if(nature != null) {
				natures.put(project.getName(),nature);
			}
			return nature;
		}
	}

	/**
	 * FIXME: rename and document!
	 * @param project
	 * @return
	 * @throws CoreException
	 */
	static public JTransformerNature getNatureIfAvailable(IProject project) throws CoreException {
		return (JTransformerNature)natures.get(project.getName());
	}
	/**
	 * FIXME: rename and document!
	 * @param project
	 * @return
	 * @throws CoreException
	 */
	public static void removeNatureFromRegistry(IProject project) {
		natures.remove(project.getName());
	}
	
	/**
	 * Check if the preference value "Create Reverse Index" is set.
	 * 
	 * @return
	 */
	public boolean useReverseIndex() {
		return Boolean.valueOf(
			plugin.getPreferenceValue(JTransformer.PREF_REVERSE_INDEX, "false")).
				  booleanValue();
	}
	
	private void reconfigureDebugOutput() throws FileNotFoundException {
		String debugLevel = getPreferenceValue(JTransformer.PREF_DEBUG_LEVEL, "WARNING");
		JTDebug.setDebugLevel(debugLevel);
		String logFileName = getPreferenceValue(JTransformer.PREF_CLIENT_LOG_FILE, null);
		if (logFileName != null && !logFileName.equals("")) {
			System.out.println("debug output is written to: " + logFileName);
			File logFile = new File(logFileName);
			BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
					new FileOutputStream(logFile, true));
			JTDebug.setOutputStream(new PrintStream(bufferedOutputStream));
		} else {
			JTDebug.setOutputStream(System.err);
		}
	}

	Set ignoreBuild = new HashSet();
	/**
	 * Make sure that the next build process is ignored.
	 * 
	 * @param project
	 * @return
	 */
	synchronized public boolean ignoreThisBuild(IProject project) {
		if(ignoreBuild.contains(project.getName())){
			ignoreBuild.remove(project.getName());
			return true;
		}
		return false;
	}
	
	synchronized public void setIgnoreThisBuild(IProject project) {
		if(ignoreBuild.contains(project.getName())){
			JTDebug.error("unexpected error in JTransformerPlugin.setIgnoreThisBuild");
		}
		ignoreBuild.add(project.getName());
	}
	
	/**
	 * Returns the subscription associated with the prolog runtime key <i>key</i>.
	 * The JTransformerSubscription is created by the first JTransformerNature requesting the Subscription.
	 *
	 * 
	 * @return returns the subscription associated with the prolog runtime key <i>key</i>. If there is no
	 *  subscription, yet the method return null.
	 */
	public static JTransformerSubscription getJTransformerSubscription(String key) {
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		synchronized (reg) {
			return (JTransformerSubscription)reg.getSubscription(JTUtils.getSubscriptionIDForRuntimeKey(key));
		}
	}

}
