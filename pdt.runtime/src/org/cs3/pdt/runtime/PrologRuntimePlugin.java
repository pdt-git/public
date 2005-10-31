package org.cs3.pdt.runtime;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.List;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
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
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;


public class PrologRuntimePlugin  extends AbstractUIPlugin {


    //The shared instance.
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

    
    private PrologInterface prologInterface;

    //Resource bundle.
    private ResourceBundle resourceBundle;

    
    private DefaultResourceFileLocator rootLocator;

    private Option[] options;

	

	

    

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
     * shortcut for <code>getPrologInterface().getConsultService(key)</code>.
     * 
     * @param key
     * @return
     * @deprecated
     */
    public ConsultService getConsultService(String key) {
        return getPrologInterface().getConsultService(key);
    }

    

    /**
     * @return the prolog interface instance shared among this plugin's
     *            components.
     * @throws IOException
     */
    private PrologInterface getPrologInterface() {
        if (prologInterface == null) {

            String impl = getPreferenceValue(PrologRuntime.PREF_PIF_IMPLEMENTATION, null);
            if (impl == null) {
                throw new RuntimeException("The required property \""
                        + PrologRuntime.PREF_PIF_IMPLEMENTATION + "\" was not specified.");
            }
            PrologInterfaceFactory factory = PrologInterfaceFactory
                    .newInstance(impl);
            factory.setResourceLocator(getResourceLocator(PrologRuntime.LOC_PIF));
            prologInterface = factory.create();            
            reconfigurePrologInterface();
            registerHooks();
            try {
                prologInterface.start();
            } catch (IOException e) {
                Debug.report(e);
                throw new RuntimeException(e);
            }
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
            //reconfigureDebugOutput();
           
            reconfigurePrologInterface();

            if(restart){
                pif.start();
            }
           
        } catch (Throwable e) {
            Debug.report(e);
        }

    }

    

    private void reconfigurePrologInterface() {

        //MetadataEngineInstaller.install(prologInterface);
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
//XXX
    public List getBootstrapList(String key) {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID,
                PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
        if (point == null) {
            Debug.error("could not find the extension point "
                    + PrologRuntime.EP_BOOTSTRAP_CONTRIBUTION);
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
                    Debug.debug("got this resname: "+resName);
                    String namespace = ext.getNamespace();
                    Debug.debug("got this namespace: "+namespace);
                    URL url = Platform.getBundle(namespace).getEntry(
                            resName);
                    try {
                        Debug.debug("trying to resolve this url: "+url);
                        url = Platform.asLocalURL(url);
                    } catch (IOException e) {
                        Debug.report(e);
                        throw new RuntimeException("Problem resolving url: "
                                + url.toString(), e);
                    }
                    //URI uri = URI.create(url.toString());                    
                    File file = new File(url.getFile());
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
        IExtensionPoint point = registry.getExtensionPoint(PrologRuntime.PLUGIN_ID,
                PrologRuntime.EP_HOOKS);
        if (point == null) {
            Debug.error("could not find the extension point " + PrologRuntime.EP_HOOKS);
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
                        PrologRuntime.PREF_PIF_IMPLEMENTATION,
                        "PrologInterfaceFactory implementation",
                        "The factory to be used for creating PrologInterface instances",
                        Option.STRING, PrologInterfaceFactory.DEFAULT)
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

   
   

    private String getLocation() throws IOException {
        URL url = getDefault().getBundle().getEntry("/");
        String location = null;
        location = new File(Platform.asLocalURL(url).getFile())
                .getAbsolutePath();
        if (location.charAt(location.length() - 1) == File.separatorChar)
            location = location.substring(0, location.length() - 1);
        return location;
    }

	
    

}
