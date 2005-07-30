package org.cs3.pdt.console;


import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.console.internal.DefaultPrologConsoleService;
import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.console.prolog.PrologConsoleService;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.preferences.IPreferencesService;
import org.eclipse.jface.resource.JFaceResources;
import org.eclipse.ui.plugin.AbstractUIPlugin;



public class PrologConsolePlugin extends AbstractUIPlugin {

	private Option[] options;
	private ResourceBundle resourceBundle;

	private void initOptions() {
        
        options = new Option[] {
        		new SimpleOption(
                        PDTConsole.PREF_CONSOLE_PORT,
                        "Console Port",
                        "Number of the port used for connecting the console to the Prolog prozess",
                        Option.NUMBER, "4711")   ,
                new SimpleOption(
                		PDTConsole.PREF_CONSOLE_FONT,
                		"Console Font",
                		"Font used in the Prolog Console view",
                		Option.FONT,JFaceResources.DEFAULT_FONT)
                              
        };

    }

	
	
//	The shared instance.
    private static PrologConsolePlugin plugin;

    /**
     * Returns the shared instance.
     */
    public static PrologConsolePlugin getDefault() {
        return plugin;
    }
	
	public Option[] getOptions(){
		if(this.options==null){
			initOptions();
		}
		return this.options;
	}

	public PrologConsolePlugin() {
		super();
        plugin = this;
        try {
            resourceBundle = ResourceBundle
                    .getBundle("prg.cs3.pdt.PDTPluginResources");
        } catch (MissingResourceException x) {
            resourceBundle = null;
        }
	}

	private PrologConsoleService consoleService;

	public PrologConsoleService getPrologConsoleService() {
		if(consoleService==null){
			consoleService=new DefaultPrologConsoleService();
		}
		return consoleService;
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

	public void reconfigure() {
		// XXX reconnect console here.
		
	}
}
