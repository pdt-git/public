package org.cs3.pdt;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.internal.PDTPrologHelper;
import org.cs3.pdt.internal.PDTServerStartStrategy;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.hooks.ConsultServerHook;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.ConsultService;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.RecordingConsultService;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.IPrologInterface;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
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
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * The main plugin class to be used in the desktop.
 */
public class PDTPlugin extends AbstractUIPlugin {

    private static final String EP_INIT_HOOK = "hooks";

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

    private HashMap consultServices = new HashMap();

    private RecordingConsultService metadataConsultService;

    private String pdtModulePrefix = "";

    private PDTPrologHelper prologHelper;

    private PrologInterface prologInterface;

    //Resource bundle.
    private ResourceBundle resourceBundle;

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
     * Retrieve a registered ConsultService instance. The Plugin may offer
     * several ConsultServices for different purposes. A service that manages
     * predicate meta information for the components of the prolog ide is always
     * be available using the key PDT.CS_METADATA
     * 
     * @param key
     * @return
     */
    public ConsultService getConsultService(String key) {
        synchronized (consultServices) {
            return (ConsultService) consultServices.get(key);
        }
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
    public void showSourceLocation(SourceLocation loc)  {
        IFile file = null;
        IWorkspace workspace = ResourcesPlugin.getWorkspace();
        IWorkspaceRoot root = workspace.getRoot();
        IPath fpath = new Path(loc.file);
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
     *               components.
     * @throws IOException
     */
    public IPrologInterface getPrologInterface() throws IOException {

        return prologInterface;
    }

    /**
     * Returns the plugin's resource bundle,
     */
    public ResourceBundle getResourceBundle() {
        return resourceBundle;
    }

    /*
     * (non-Javadoc)
     * 
     * @see prg.cs3.pdt.PreferenceListener#preferencesChanged(prg.cs3.pdt.PreferencesEvent)
     */
    protected void preferenceChanged(PropertyChangeEvent e) {
        String key = e.getProperty();
        if (key.equals(PDT.PREF_SERVER_PORT) || key.equals(PDT.PREF_SWIPL_DIR)
                || key.equals(PDT.PREF_SERVER_CLASSPATH)
                || key.equals(PDT.PREF_USE_SESSION_POOLING)
                || key.equals(PDT.PREF_DEBUG_LEVEL)
                || key.equals(PDT.PREF_SERVER_STANDALONE)) {
            try {
                prologInterface.stop();
                reconfigurePrologInterface();
                prologInterface.start();
            } catch (IOException e1) {
                Debug.report(e1);
            }
        }

    }

    /**
     *  
     */
    public void reconfigure() {
        try {
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = getBundle().getSymbolicName();
        String debugLevel = service.getString(qualifier, PDT.PREF_DEBUG_LEVEL,
                "ERROR", null);
        Debug.setDebugLevel(debugLevel);
        prologInterface.stop();
        reconfigurePrologInterface();
        reconfigureMetaDataConsultService();
        
            prologInterface.start();
        } catch (Throwable e) {
            Debug.report(e);
        }

    }

    /**
     *  
     */
    private void reconfigureMetaDataConsultService() {
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = getBundle().getSymbolicName();

        int port = service.getInt(qualifier, PDT.PREF_CONSULT_PORT, -1, null);
        if (port == -1l) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_CONSULT_PORT + "\" was not specified.");
        }

        String prefix = service.getString(qualifier,
                PDT.PREF_METADATA_STORE_DIR, null, null);
        if (prefix == null) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_METADATA_STORE_DIR + "\" was not specified.");
        }

        metadataConsultService.setPort(port);
        metadataConsultService.setPrefix(new File(prefix));
        metadataConsultService.setPrologInterface(prologInterface);

    }

    private void reconfigurePrologInterface() {
        IPreferencesService service = Platform.getPreferencesService();
        String qualifier = getBundle().getSymbolicName();
        int port = service.getInt(qualifier, PDT.PREF_SERVER_PORT, -1, null);
        if (port == -1l) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_SERVER_PORT + "\" was not specified.");
        }

        String swiHome = service.getString(qualifier, PDT.PREF_SWIPL_DIR, null,
                null);
        if (swiHome == null) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_SWIPL_DIR + "\" was not specified.");
        }
        String classPath = service.getString(qualifier,
                PDT.PREF_SERVER_CLASSPATH, null, null);
        if (classPath == null) {
            throw new NullPointerException("Required property \""
                    + PDT.PREF_SERVER_CLASSPATH + "\" was not specified.");
        }
        String debugLevel = service.getString(qualifier, PDT.PREF_DEBUG_LEVEL,
                null, null);
        if (debugLevel == null) {
            Debug.warning("The property \"" + PDT.PREF_DEBUG_LEVEL
                    + "\" was not specified." + "Assuming default: ERROR");
            debugLevel = "ERROR";
        }

        boolean standalone = service.getBoolean(qualifier,
                PDT.PREF_SERVER_STANDALONE, false, null);
        boolean pooling = service.getBoolean(qualifier,
                PDT.PREF_USE_SESSION_POOLING, false, null);

        Debug.info("configuring PrologInterface instance:");
        Debug.info("\t" + PDT.PREF_SERVER_PORT + " = " + port);
        Debug.info("\t" + PDT.PREF_SERVER_STANDALONE + " = " + standalone);
        Debug.info("\t" + PDT.PREF_SERVER_CLASSPATH + " = " + classPath);
        Debug.info("\t" + PDT.PREF_USE_SESSION_POOLING + " = " + pooling);
        Debug.info("\t" + PDT.PREF_SWIPL_DIR + " = " + swiHome);
        Debug.info("\t" + PDT.PREF_DEBUG_LEVEL + " = " + debugLevel);

        prologInterface.setPort(port);
        prologInterface.setStandAloneServer(standalone);
        prologInterface.setUseSessionPooling(pooling);
        prologInterface.setStartStrategy(new PDTServerStartStrategy(swiHome,
                classPath, debugLevel));

    }

    public void registerConsultService(String key, ConsultService consultService) {
        synchronized (consultServices) {
            consultServices.put(key, consultService);
        }
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
                EP_INIT_HOOK);
        if (point == null) {
            Debug.error("could not find the extension point " + EP_INIT_HOOK);
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

            prologInterface = new PrologInterface();
            reconfigurePrologInterface();
            metadataConsultService = new RecordingConsultService();
            reconfigureMetaDataConsultService();
            LifeCycleHook hook = new LifeCycleHook() {
                public void onInit(PrologSession initSession) {
                    try {
                        metadataConsultService.connect();
                        
                    } catch (IOException e) {
                        Debug.report(e);
                    }
                }

                public void afterInit() {
                    try{
                        metadataConsultService.reload();
                    }
                    catch(Throwable t){
                        Debug.report(t);
                    }
                }

                public void beforeShutdown(PrologSession session) {
                    metadataConsultService.disconnect();
                }
            };            
            prologInterface.addLifeCycleHook(hook, "metadataConsultService",
                    new String[] { ConsultServerHook.HOOK_ID });
            
            registerConsultService(PDT.CS_METADATA,metadataConsultService);
            registerHooks();
            prologInterface.start();
        } catch (Throwable t) {
            Debug.report(t);
        }
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

    //    public IFile getActiveFile() {
    //		try {
    //			IEditorPart editorPart = getActiveEditor();
    //			return ((FileEditorInput)editorPart.getEditorInput()).getFile();
    //		}catch (NullPointerException e){ return null; }
    //	}
    //
    //	public String getActiveFileName() {
    //		try {
    //			return getActiveFile().getFullPath().toString();
    //		}catch (NullPointerException e){ return null; }
    //	}
    //	
    //	
    //	public String getActiveRawFileName() {
    //		try {
    //			return getActiveFile().getRawLocation().toFile().getAbsolutePath();
    //		}catch (NullPointerException e){ return null; }
    //
    //	}

}