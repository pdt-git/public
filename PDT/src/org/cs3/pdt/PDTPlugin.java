package org.cs3.pdt;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URI;
import java.net.URL;
import java.util.HashMap;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;

import org.cs3.pdt.internal.PDTPrologHelper;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.views.PrologNode;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.MetadataEngineInstaller;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin implements IAdaptable {

    public static final String MODULEPREFIX = "pdtplugin:";

    //The shared instance.
    private static PDTPlugin plugin;

    /**
     * Returns the shared instance.
     */
    public static PDTPlugin getDefault() {
        return plugin;
    }

    /**
     * Returns the string from the plugin's resource bundle, or 'key' if not
     * found.
     */
    public static String getResourceString(String key) {
        ResourceBundle bundle = PDTPlugin.getDefault().getResourceBundle();
        try {
            return (bundle != null) ? bundle.getString(key) : key;
        } catch (MissingResourceException e) {
            return key;
        }
    }

    private String pdtModulePrefix = "";

    private PDTPrologHelper prologHelper;

    private PrologInterface prologInterface;

    //Resource bundle.
    private ResourceBundle resourceBundle;

    private Object root;

    private DefaultResourceFileLocator rootLocator;

    private Option[] options;

    //private HashMap locators=new HashMap();

    /**
     * The constructor.
     */
    public PDTPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("prg.cs3.pdt.PDTPluginResources");
        } catch (MissingResourceException x) {
            resourceBundle = null;
        }
    }

    public ResourceFileLocator getResourceLocator(String key) {
        if (rootLocator == null) {
            URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
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

    public IEditorPart getActiveEditor() {
        try {
            IWorkbenchPage page = getActivePage();
            return page.getActiveEditor();
        } catch (NullPointerException e) {
            return null;
        }
    }

    public IWorkbenchPage getActivePage() {
        return getWorkbench().getActiveWorkbenchWindow().getActivePage();
    }

    /**
     * shortcut for <code>getPrologInterface().getConsultService(key)</code>.
     * 
     * @param key
     * @return
     */
    public ConsultService getConsultService(String key) {
        return getPrologInterface().getConsultService(key);
    }

    public Display getDisplay() {
        return getWorkbench().getDisplay();
    }

    public IMetaInfoProvider getMetaInfoProvider() {
        if (prologHelper == null) {
            prologHelper = new PDTPrologHelper(prologInterface, pdtModulePrefix);
        }
        return prologHelper;
    }

    public void showSourceLocation(SourceLocation loc) {
        IFile file = null;
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();
        IPath fpath;
        try {
            fpath = new Path(new File(loc.file).getCanonicalPath());
        } catch (IOException e1) {
            Debug.report(e1);
            return;
        }
        IFile[] files = root.findFilesForLocation(fpath);
        if (files == null || files.length == 0) {
            Debug.warning("Not in Workspace: " + fpath);
            return;
        }
        if (files.length > 1) {
            Debug.warning("Mapping into workspace is ambiguose:" + fpath);
            Debug.warning("i will use the first match found: " + files[0]);
        }
        file = files[0];
        if (!file.isAccessible()) {
            Debug.warning("The specified file \"" + file
                    + "\" is not accessible.");
            return;
        }
        IWorkbenchPage page = PDTPlugin.getDefault().getActivePage();
        IEditorPart part;
        try {
            part = IDE.openEditor(page, file);
        } catch (PartInitException e) {
            Debug.report(e);
            return;
        }
        if (part instanceof PLEditor) {
            PLEditor editor = (PLEditor) part;
            editor.gotoLine(loc.line);
        }
    }

    /**
     * @return the prolog interface instance shared among this plugin's
     *            components.
     * @throws IOException
     */
    public PrologInterface getPrologInterface() {
        if (prologInterface == null) {

            String impl = getPreferenceValue(PDT.PREF_PIF_IMPLEMENTATION, null);
            if (impl == null) {
                throw new RuntimeException("The required property \""
                        + PDT.PREF_PIF_IMPLEMENTATION + "\" was not specified.");
            }
            PrologInterfaceFactory factory = PrologInterfaceFactory
                    .newInstance(impl);
            factory.setResourceLocator(getResourceLocator(PDT.LOC_PIF));
            prologInterface = factory.create();

            reconfigurePrologInterface();
        }
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
        PrologInterface pif = getPrologInterface();
        boolean restart=false;
        if(!pif.isDown()){
            try {
                restart=true;
                pif.stop();
            } catch (IOException e1) {
                Debug.report(e1);                
            }
        }
        try {
            String debugLevel = getPreferenceValue(PDT.PREF_DEBUG_LEVEL,
                    "WARNING");
            Debug.setDebugLevel(debugLevel);
            String logFileName = getPreferenceValue(PDT.PREF_CLIENT_LOG_FILE,
                    null);
            if(logFileName!=null && ! logFileName.equals("")){
                File logFile =  new File(logFileName);
                BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
                        new FileOutputStream(logFile, true));
                Debug.setOutputStream(new PrintStream(bufferedOutputStream));
            }
           
            reconfigurePrologInterface();

            if(restart){
                pif.start();
            }
           
        } catch (Throwable e) {
            Debug.report(e);
        }

    }

    private void reconfigurePrologInterface() {

        MetadataEngineInstaller.install(prologInterface);
        List l = prologInterface.getBootstrapLibraries();
        l.addAll(getBootstrapList(null));

        //		we are using the bootstrapContribution extension point just as
        // anybody else.
        //        --lu
        //        l.add(Util.prologFileName(getResourceLocator(PDT.LOC_ENGINE).resolve(
        //                "main.pl")));
        PrologInterfaceFactory factory = prologInterface.getFactory();
        Option[] options = factory.getOptions();
        for (int i = 0; i < options.length; i++) {
            String id = options[i].getId();
            String val = getPreferenceValue(id, options[i].getDefault());
            prologInterface.setOption(id, val);
        }

    }

    public List getBootstrapList(String key) {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint("org.cs3.pdt",
                PDT.EP_BOOTSTRAP_CONTRIBUTION);
        if (point == null) {
            Debug.error("could not find the extension point "
                    + PDT.EP_BOOTSTRAP_CONTRIBUTION);
            return null;
        }
        IExtension[] extensions = point.getExtensions();
        List r = new Vector();
        for (int i = 0; i < extensions.length; i++) {
            IExtension ext = extensions[i];
            IConfigurationElement[] configurationElements = ext
                    .getConfigurationElements();
            for (int j = 0; j < configurationElements.length; j++) {
                IConfigurationElement elm = configurationElements[j];
                String resName = elm.getAttribute("path");
                String className = elm.getAttribute("class");
                if (className != null) {
                    BootstrapContribution bc = null;
                    try {
                        bc = (BootstrapContribution) elm
                                .createExecutableExtension("class");
                        bc.contributeToBootstrapList(key, r);
                    } catch (CoreException e1) {
                        Debug.report(e1);
                        throw new RuntimeException("Problem instantiating: "
                                + elm.getAttributeAsIs("class"), e1);
                    }
                } else if (resName != null) {
                    URL url = Platform.getBundle(ext.getNamespace()).getEntry(
                            resName);
                    try {
                        url = Platform.resolve(url);
                    } catch (IOException e) {
                        Debug.report(e);
                        throw new RuntimeException("Problem resolving url: "
                                + url.toString(), e);
                    }
                    URI uri = URI.create(url.toString());
                    File file = new File(uri);
                    r.add(Util.prologFileName(file));
                }
            }
        }
        return r;
    }

    /**
     * Looks up all avaible extensions for the extension point
     * org.cs3.pl.extension.factbase.updated, creates Observer objects and calls
     * their update() methods.
     * 
     * @param project
     * @param prologManager
     * 
     * @throws CoreException
     */
    protected boolean registerHooks() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint("org.cs3.pdt",
                PDT.EP_HOOKS);
        if (point == null) {
            Debug.error("could not find the extension point " + PDT.EP_HOOKS);
            return false;
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
                        prologInterface
                                .addLifeCycleHook(hook, id, dependencies);
                    }
                }
            }
        } catch (CoreException e) {
            Debug.report(e);
            return false;
        }
        return true;
    }

    public void setStatusErrorMessage(final String string) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                getActiveEditor().getEditorSite().getActionBars()
                        .getStatusLineManager().setErrorMessage(string);
            }
        });
    }

    public void setStatusMessage(final String string) {
        getDisplay().asyncExec(new Runnable() {
            public void run() {
                getActiveEditor().getEditorSite().getActionBars()
                        .getStatusLineManager().setMessage(string);
            }
        });

    }

    /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        try {
            super.start(context);
            initOptions();

            /*
             * XXX not sure if this is the "eclipse way(tm)" to go, but we need
             * to make sure that the pdt preferences are correctly initilized
             * before proceeding.
             */
            getPluginPreferences();
            reconfigure();
            PrologInterface pif = getPrologInterface();
            ConsultService metadataConsultService = pif
                    .getConsultService(PDT.CS_METADATA);
            //metadataConsultService.setRecording(true);
            registerHooks();
            prologInterface.start();
        } catch (Throwable t) {
            Debug.report(t);
        }
    }

    public Option[] getOptions() {
        return this.options;
    }

    /**
     *  
     */
    private void initOptions() {
        String fileSep = File.separator;
        String pathSep = File.pathSeparator;
        String location = "";
        try {
            location = getLocation();
        } catch (IOException e) {
            Debug.report(e);
            Debug.error("Could not find plugin installation dir.");
        }

        options = new Option[] {
                new SimpleOption(
                        PDT.PREF_CONSOLE_PORT,
                        "Console Port",
                        "Number of the port used for connecting the console to the Prolog prozess",
                        Option.NUMBER, "4711"),
                new SimpleOption(PDT.PREF_DEBUG_LEVEL, "Debug Level",
                        "Determines the verbosity of the debug log file.",
                        Option.ENUM, "WARNING", new String[][] {
                                { "error", "ERROR" }, { "warning", "WARNING" },
                                { "info", "INFO" }, { "debug", "DEBUG" } }),
                new SimpleOption(
                        PDT.PREF_METADATA_ENGINE_DIR,
                        "Metadata Engine Dir",
                        "Directory containing the prolog implementation of the meta data engine,",
                        Option.DIR, location + fileSep + "engine"),
                new SimpleOption(PDT.PREF_METADATA_STORE_DIR,
                        "Metadata Store Dir",
                        "Directory used to store metadata for prolog files.",
                        Option.DIR, location + fileSep + "store"),
                new SimpleOption(
                        PDT.PREF_SOURCE_PATH_DEFAULT,
                        "Default Source Path",
                        "The default value for the source path of prolog projects.",
                        Option.STRING, "/pl"),
                new SimpleOption(
                        PDT.PREF_PIF_IMPLEMENTATION,
                        "PrologInterfaceFactory implementation",
                        "The factory to be used for creating PrologInterface instances",
                        Option.STRING, PrologInterfaceFactory.DEFAULT),
                new SimpleOption(
                        PDT.PREF_CLIENT_LOG_FILE,
                        "Log file location",
                        "A file to which debug output of the PDT will be writen",
                        Option.FILE, location + fileSep + "pdt.log")

        };

    }

    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        try {
            if (prologInterface != null && !prologInterface.isDown()) {
                prologInterface.stop();
            }
        } finally {
            super.stop(context);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
     */
    public Object getAdapter(Class adapter) {
        if (IWorkbenchAdapter.class.equals(adapter)) {
            if (root == null) {
                root = new IWorkbenchAdapter() {
                    private Object[] modules;

                    public Object[] getChildren(Object o) {
                        if (modules == null) {
                            modules = PrologNode.find(getPrologInterface(),
                                    "type(module)").toArray();
                        }
                        return modules;
                    }

                    public ImageDescriptor getImageDescriptor(Object object) {
                        String imageKey = ISharedImages.IMG_OBJ_ELEMENT;
                        return PlatformUI.getWorkbench().getSharedImages()
                                .getImageDescriptor(imageKey);
                    }

                    public String getLabel(Object o) {
                        return "Nur wo PDT drauf steht, ist auch PDT drin.";
                    }

                    public Object getParent(Object o) {
                        return null;
                    }
                };
            }
            return root;
        }
        return null;
    }

    /**
     * Retrieve a PrologInterface for this Project.
     * 
     * <p>
     * rational: we considered to have per project PIFs or at least some kind
     * namespace separation between projects. This is my desparate try to
     * reflect such considrations api-wise.
     * <p>
     * NOTE: per-project PIFs are not implemented yet,so you will aways get the
     * same PIF, but you should use this none the less when you are dealing with
     * data that can be associated to a concrete project.
     * 
     * @return the PIF for this Project.
     * @param project
     * @return
     */
    public PrologInterface getPrologInterface(String key) {
        return getPrologInterface();
    }

    private String getLibDir() {
        String proj;
        String cp;

        String sep = System.getProperty("file.separator");
        try {
            if (getLocation() != null) {
                return getLocation() + sep + "lib";
            }
        } catch (IOException e) {
            // ok, then the other way
        }

        String resName = "org/cs3/pdt/PDTPlugin.class";
        URL url = ClassLoader.getSystemClassLoader().getResource(resName);
        String path = url.getFile();

        return path.substring(0, path.indexOf(resName)) + sep + "lib";
    }

    private String getLocation() throws IOException {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
        String location = null;
        location = new File(Platform.asLocalURL(url).getFile())
                .getAbsolutePath();
        if (location.charAt(location.length() - 1) == File.separatorChar)
            location = location.substring(0, location.length() - 1);
        return location;
    }

    private String guessExecutableName() {
        String osname = System.getProperty("os.name");
        if (osname.indexOf("Windows") > -1) {
            return "plwin";
        }
        return "xpce";
    }

}