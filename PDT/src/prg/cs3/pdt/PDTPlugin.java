package prg.cs3.pdt;

import java.io.IOException;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;

import org.cs3.pdt.PDTServerStartStrategy;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Properties;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;


/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin implements PreferenceListener {
	//The shared instance.
	private static PDTPlugin plugin;
	//Resource bundle.
	private ResourceBundle resourceBundle;
	private PrologInterface prologInterface;
	private IPreferences preferences;
	private static final String EP_INIT_HOOK = "hooks.init";
	
	/**
	 * The constructor.
	 */
	public PDTPlugin() {
		super();
		plugin = this;
		try {
			resourceBundle = ResourceBundle.getBundle("prg.cs3.pdt.PDTPluginResources");
		} catch (MissingResourceException x) {
			resourceBundle = null;
		}
	}

	/**
	 * This method is called upon plug-in activation
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		preferences = new Preferences();
	}

	/**
	 * This method is called when the plug-in is stopped
	 */
	public void stop(BundleContext context) throws Exception {
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 */
	public static PDTPlugin getDefault() {
		return plugin;
	}

	/**
	 * Returns the string from the plugin's resource bundle,
	 * or 'key' if not found.
	 */
	public static String getResourceString(String key) {
		ResourceBundle bundle = PDTPlugin.getDefault().getResourceBundle();
		try {
			return (bundle != null) ? bundle.getString(key) : key;
		} catch (MissingResourceException e) {
			return key;
		}
	}

	/**
	 * Returns the plugin's resource bundle,
	 */
	public ResourceBundle getResourceBundle() {
		return resourceBundle;
	}

	/**
	 * @return the prolog interface instance shared among this plugin's components.
	 * @throws IOException
	 */
	public PrologInterface getPrologInterface() throws IOException {
		if(prologInterface==null){
			prologInterface = new PrologInterface();
			int port = Integer.parseInt(preferences.get(Properties.SERVER_PORT,"1414"));
			preferences.addPreferencesListener(this);
		}
		return prologInterface;
	}

	
	public IPreferences getPreferences() {
		return preferences;
	}

	/* (non-Javadoc)
	 * @see prg.cs3.pdt.PreferenceListener#preferencesChanged(prg.cs3.pdt.PreferencesEvent)
	 */
	public void preferencesChanged(PreferencesEvent e) {
		Set keys = e.getKeys();
		if(keys.contains(Properties.SERVER_PORT)
				||keys.contains(Properties.ENGINE_DIR)
				||keys.contains(Properties.SERVER_CLASSPATH)
				||keys.contains(Properties.DEBUG_LEVEL)
				||keys.contains(Properties.SERVER_STANDALONE)){
			try {
				prologInterface.stop();
				reconfigurePrologInterface();
				prologInterface.start();
			} catch (IOException e1) {
				Debug.report(e1);
			}
		}
		
		
	}

	private void reconfigurePrologInterface() {
		String swiHome = preferences.get(Properties.SWIPL_DIR);
		if(swiHome==null){
			throw new NullPointerException("Required property \""+Properties.SWIPL_DIR+"\" was not specified.");
		}
		String classPath = preferences.get(Properties.SERVER_CLASSPATH);			
		if(classPath==null){
			throw new NullPointerException("Required property \""+Properties.SERVER_CLASSPATH+"\" was not specified.");
		}
		int port = Integer.parseInt(preferences.get(Properties.SERVER_PORT,"1414"));
		String debugLevel = preferences.get(Properties.DEBUG_LEVEL,"ERROR");
		boolean standalone=Boolean.getBoolean(preferences.get(Properties.SERVER_STANDALONE,"false"));
		boolean pooling=Boolean.getBoolean(preferences.get(Properties.USE_SESSION_POOLING,"false"));
		prologInterface.setPort(port);		
		prologInterface.setStandAloneServer(standalone);
		prologInterface.setUseSessionPooling(pooling);
		prologInterface.setStartStrategy(new PDTServerStartStrategy(swiHome,classPath,debugLevel));
		
	}
	/**
     * Looks up all avaible extensions for the extension point  
     * org.cs3.pl.extension.factbase.updated, creates Observer objects
     * and calls their update() methods. 
     * @param project
     * @param prologManager
     * 
     * @throws CoreException
     */
    public boolean registerHooks() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(
                "org.cs3.pl.JTransformer", EP_INIT_HOOK);
        if (point == null) {
            Debug.error("could not find the extension point "
                    + EP_INIT_HOOK);
            return false;
        }
        IExtension[] extensions = point.getExtensions();
        try {
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] celem = extensions[i]
                        .getConfigurationElements();
                if(! celem[0].getName().equals("hook")){
                	throw new RuntimeException("hmmm... asumed a hook, but got a "+celem[0].getName());
                }
                LifeCycleHook hook = (LifeCycleHook) celem[0].createExecutableExtension("class");
                String dependsOn= celem[0].getAttributeAsIs("dependsOn");
                if(dependsOn==null){
                	dependsOn="";
                }
                String[] dependencies = dependsOn.split(",");
                String id= celem[0].getAttributeAsIs("id");
                prologInterface.addLifeCycleHook(hook,id,dependencies);
                
            }
        } catch (CoreException e) {
            Debug.report(e);
            return false;
        }
        return true;
    }

}
