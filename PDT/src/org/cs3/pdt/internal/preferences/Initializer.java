/*
 */
package org.cs3.pdt.internal.preferences;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URL;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.core.runtime.preferences.DefaultScope;
import org.eclipse.core.runtime.preferences.IEclipsePreferences;
import org.eclipse.core.runtime.preferences.IScopeContext;
import org.osgi.service.prefs.BackingStoreException;

/**
 * this class tries to guess sensible defaults for most property values.
 */
public class Initializer extends AbstractPreferenceInitializer {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer#initializeDefaultPreferences()
     */
    public void initializeDefaultPreferences() {
        try {
            initializeDefaultPreferences_impl();
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t.getMessage(), t);
        }
    }

    /**
     * @throws BackingStoreException
     * @throws IOException
     * @throws InterruptedException
     *  
     */
    private void initializeDefaultPreferences_impl()
            throws BackingStoreException, IOException, InterruptedException {
        PDTPlugin plugin = PDTPlugin.getDefault();

        String qualifier = plugin.getBundle().getSymbolicName();

        IScopeContext context = new DefaultScope();

        IEclipsePreferences node = context.getNode(qualifier);
        if (node == null) {
            Debug.error("Häh?!");
        } else {
            Option[] options = plugin.getOptions();
            for (int i = 0; i < options.length; i++) {
                String id = options[i].getId();
                String def = System.getProperty(id, options[i].getDefault());
                node.put(id, def);
            }
            String pifImpl=plugin.getPreferenceValue(PDT.PREF_PIF_IMPLEMENTATION,null);
            PrologInterfaceFactory factory = PrologInterfaceFactory
                    .newInstance(pifImpl);
            factory.setResourceLocator(plugin.getResourceLocator(PDT.LOC_PIF));
             options = factory.getOptions();
            for (int i = 0; i < options.length; i++) {
                String id = options[i].getId();
                String def = System.getProperty(id, options[i].getDefault());
                node.put(id, def);
            }
            String[] strings = node.keys();
            for (int i = 0; i < strings.length; i++) {
                Debug.info(strings[i] + " --> " + node.get(strings[i], "n.a."));
            }
        }
        
    }
}