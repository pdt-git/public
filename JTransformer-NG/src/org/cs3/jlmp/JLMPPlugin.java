/*
 */
package org.cs3.jlmp;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.common.ResourceFileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.ui.plugin.AbstractUIPlugin;

/**
 * The Java Logical Meta-Programming.(aka JTransformer) Plugin.
 */
public class JLMPPlugin extends AbstractUIPlugin {

   

    public static final String PLUGIN_ID = "org.cs3.jlmp";
    private static JLMPPlugin plugin;
    private ResourceBundle resourceBundle;
    private ResourceFileLocator rootLocator;

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
     * <code>rootLocator.subLocator(key)</code>
     * where root locator "points" to the installation directory of the
     * plugin.
     * @param key should be a valid filesystem path element.
     * @return a resource file locator 
     */
    public ResourceFileLocator getResourceLocator(String key) {
        if (rootLocator == null) {
            URL url = getBundle().getEntry("/");
            String location = null;
            try {
                location = new File(Platform.asLocalURL(url).getFile())
                        .getAbsolutePath();
            } catch (IOException t) {
                Debug.report(t);
                throw new RuntimeException(t);
            }
            if (location.charAt(location.length() - 1) == File.separatorChar) {
                location = location.substring(0, location.length() - 1);
            }
            rootLocator = new DefaultResourceFileLocator(location);
        }
        return rootLocator.subLocator(key);
    }
    
   
    
    
}
