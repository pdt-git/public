package org.cs3.pdt.core;


import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.metadata.DefaultMetaInfoProvider;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.ui.plugin.AbstractUIPlugin;

public class PDTCorePlugin extends AbstractUIPlugin {

	private ResourceBundle resourceBundle;
	private DefaultMetaInfoProvider prologHelper;
	private String pdtModulePrefix = "";
	private Option[] options;
    public IMetaInfoProvider getMetaInfoProvider() {
        if (prologHelper == null) {
            prologHelper = new DefaultMetaInfoProvider(PrologRuntimePlugin.getDefault().getPrologInterface(), pdtModulePrefix);
        }
        return prologHelper;
    }
	public Option[] getOptions() {
		if (options == null) {
			initOptions();
		}
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
		String value = service.getString(qualifier, key, defaultValue, null);
		return System.getProperty(key, value);
	}
	private String getLocation() throws IOException {
		URL url = PDTCorePlugin.getDefault().getBundle().getEntry("/");
		String location = null;
		location = new File(Platform.asLocalURL(url).getFile())
				.getAbsolutePath();
		if (location.charAt(location.length() - 1) == File.separatorChar)
			location = location.substring(0, location.length() - 1);
		return location;
	}
	
	private String ensureDirExists(String value, String label) {
		if (value == null) {
			return label + "must not be null";
		}
		if (value.length() == 0) {
			return label + " must not be empty";
		}
		File f = new File(value);
		if (!f.isAbsolute()) {
			return label + " must not be an absolute path";
		}
		if (!f.exists()) {
			if (!f.mkdirs()) {
				return "could not create " + label;
			}
			return "";
		}
		if (!f.isDirectory()) {
			return label + " exists, but is not a directory";
		}
		if (!f.canWrite()) {
			return label + " exists, but is not writable";
		}
		return "";
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
						PDTCore.PREF_METADATA_ENGINE_DIR,
						"Metadata Engine Dir",
						"Directory containing the prolog implementation of the meta data engine,",
						Option.DIR, location + fileSep + "engine") {
	
					public String validate(String value) {
						return ensureDirExists(value, "Metadata Engine Dir");
					}

					
	
				},
				new SimpleOption(PDTCore.PREF_METADATA_STORE_DIR,
						"Metadata Store Dir",
						"Directory used to store metadata for prolog files.",
						Option.DIR, location + fileSep + "store") {
					public String validate(String value) {
						return ensureDirExists(value, "Metadata Store Dir");
					}
				},
				new SimpleOption(
						PDTCore.PREF_SOURCE_PATH_DEFAULT,
						"Default Source Path",
						"The default value for the source path of prolog projects.",
						Option.STRING, "/"),
				new SimpleOption(
						PDTCore.PREF_AUTO_CONSULT,
						"Enable Auto-Consult (EXPERIMENTAL)",
						"If this flag is set, the PDT will automaticaly (re-)consult any source file,"
								+ "unless it is explicitly exluded from Auto-Consult. Note that this is an experimental "
								+ "feature and defaults to \"false\" for 0.1.x",
						Option.FLAG, "false")
	
		};
	
	}
	/**
	 * 
	 */
	public void reconfigure() {
		;	
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
