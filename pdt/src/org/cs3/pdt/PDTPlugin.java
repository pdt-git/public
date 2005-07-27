package org.cs3.pdt;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.internal.DefaultPrologConsoleService;
import org.cs3.pdt.internal.views.PrologNode;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;
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

    private DefaultMetaInfoProvider prologHelper;

    

    //Resource bundle.
    private ResourceBundle resourceBundle;

    private Object root;

    private DefaultResourceFileLocator rootLocator;

    private Option[] options;

	private PrologConsoleService consoleService;

	

    

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

    
    public IMetaInfoProvider getMetaInfoProvider() {
        if (prologHelper == null) {
            prologHelper = new DefaultMetaInfoProvider(PrologRuntimePlugin.getDefault().getPrologInterface(), pdtModulePrefix);
        }
        return prologHelper;
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
        try {
            reconfigureDebugOutput();
                             
        } catch (Throwable e) {
            Debug.report(e);
        }

    }

    private void reconfigureDebugOutput() throws FileNotFoundException {
        String debugLevel = getPreferenceValue(PDT.PREF_DEBUG_LEVEL,
                "WARNING");
        Debug.setDebugLevel(debugLevel);
        String logFileName = getPreferenceValue(PDT.PREF_CLIENT_LOG_FILE,
                null);
        if(logFileName!=null && ! logFileName.equals("")){
            System.out.println("debug output is written to: "+logFileName);
            File logFile =  new File(logFileName);
            BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(
                    new FileOutputStream(logFile, true));
            Debug.setOutputStream(new PrintStream(bufferedOutputStream));
        }else{
            Debug.setOutputStream(System.err);
        }
    }

        /**
     * This method is called upon plug-in activation
     */
    public void start(BundleContext context) throws Exception {
        try {
            super.start(context);
            reconfigureDebugOutput();
            
        } catch (Throwable t) {
            Debug.report(t);
        }
    }

    public Option[] getOptions() {
        if(options==null){
            initOptions();
        }
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
                        Option.STRING, "/"),
                new SimpleOption(
                        PDT.PREF_CLIENT_LOG_FILE,
                        "Log file location",
                        "A file to which debug output of the PDT will be writen",
                        Option.FILE, location + fileSep + "pdt.log"),
                new SimpleOption(
                        PDT.PREF_AUTO_CONSULT,
                        "Enable Auto-Consult (EXPERIMENTAL)",
                        "If this flag is set, the PDT will automaticaly (re-)consult any source file," +
                        "unless it is explicitly exluded from Auto-Consult. Note that this is an experimental " +
                        "feature and defaults to \"false\" for 0.1.x",
                        Option.FLAG, "false"
                        )
                        
        };

    }

    /**
     * This method is called when the plug-in is stopped
     */
    public void stop(BundleContext context) throws Exception {
        try {
            if (PrologRuntimePlugin.getDefault().getPrologInterface() != null && !PrologRuntimePlugin.getDefault().getPrologInterface().isDown()) {
                PrologRuntimePlugin.getDefault().getPrologInterface().stop();
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

                    private PrologInterface getPrologInterface() {
						return PrologRuntimePlugin.getDefault().getPrologInterface();
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

   
    private String getLocation() throws IOException {
        URL url = PDTPlugin.getDefault().getBundle().getEntry("/");
        String location = null;
        location = new File(Platform.asLocalURL(url).getFile())
                .getAbsolutePath();
        if (location.charAt(location.length() - 1) == File.separatorChar)
            location = location.substring(0, location.length() - 1);
        return location;
    }

	public PrologConsoleService getPrologConsoleService() {
		if(consoleService==null){
			consoleService=new DefaultPrologConsoleService();
		}
		return consoleService;
	}

    

}