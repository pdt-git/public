package org.cs3.pdt.core;


import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.eclipse.core.runtime.Plugin;

public class PDTCorePlugin extends Plugin {

	private ResourceBundle resourceBundle;
	private DefaultMetaInfoProvider prologHelper;
	private String pdtModulePrefix = "";
    public IMetaInfoProvider getMetaInfoProvider() {
        if (prologHelper == null) {
            prologHelper = new DefaultMetaInfoProvider(PrologRuntimePlugin.getDefault().getPrologInterface(), pdtModulePrefix);
        }
        return prologHelper;
    }
	public PDTCorePlugin() {
		super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("prg.cs3.pdt.PDTPluginResources");
        } catch (MissingResourceException x) {
            resourceBundle = null;
        }
	}

//	The shared instance.
    private static PDTCorePlugin plugin;

    /**
     * Returns the shared instance.
     */
    public static PDTCorePlugin getDefault() {
        return plugin;
    }
}
