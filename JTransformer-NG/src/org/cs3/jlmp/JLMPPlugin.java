/*
 */
package org.cs3.jlmp;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.Iterator;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * The Java Logical Meta-Programming.(aka JTransformer) Plugin.
 */
public class JLMPPlugin extends AbstractUIPlugin{

    private static JLMPPlugin plugin;

    private ResourceBundle resourceBundle;

    private ResourceFileLocator rootLocator;

    private Vector projectlisteners=new Vector();

    public JLMPPlugin() {
        super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("prg.cs3.pdt.PDTPluginResources");
        } catch (MissingResourceException x) {
            resourceBundle = null;
        }
    }

    /**
     * Returns the shared instance.
     */
    public static JLMPPlugin getDefault() {
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
            URL url = getBundle().getEntry("/");
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
     *  
     */
    private void collectListeners() {
        IExtensionRegistry registry = Platform.getExtensionRegistry();
        IExtensionPoint point = registry.getExtensionPoint("org.cs3.jlmp",
                JLMP.EP_PROJECT_LISTENER);
        if (point == null) {
            Debug.error("could not find the extension point " +  JLMP.EP_PROJECT_LISTENER);
            return ;
        }
        IExtension[] extensions = point.getExtensions();
        try {
            for (int i = 0; i < extensions.length; i++) {
                IConfigurationElement[] celem = extensions[i]
                        .getConfigurationElements();
                for (int j = 0; j < celem.length; j++) {

                    if (!celem[j].getName().equals("listener")) {
                        Debug.warning("hmmm... asumed a listener, but got a "
                                + celem[j].getName());
                    } else {
                       JLMPProjectListener listener = (JLMPProjectListener) celem[j]
                                .createExecutableExtension("class");
                        projectlisteners.add(listener);
                    }
                }
            }
        } catch (CoreException e) {
            Debug.report(e);            
        }
    }

   
    public  void fireFactBaseUpdated(JLMPProjectEvent e){        
        Vector cloned = null;
        synchronized(projectlisteners){
            cloned = (Vector) projectlisteners.clone();
        }
        for (Iterator it = cloned.iterator(); it.hasNext();) {
            JLMPProjectListener l = (JLMPProjectListener) it.next();
            l.factBaseUpdated(e);
        }
    }
}
