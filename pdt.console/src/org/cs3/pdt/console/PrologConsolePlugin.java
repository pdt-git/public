package org.cs3.pdt.console;


import java.io.File;
import java.io.IOException;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.cs3.pdt.console.internal.DefaultPrologConsoleService;
import org.cs3.pl.common.Debug;
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
                		PDTConsole.PREF_CONSOLE_FONT,
                		"Console Font",
                		"Font used in the Prolog Console view",
                		Option.FONT,JFaceResources.DEFAULT_FONT),
                new SimpleOption(
                		PDTConsole.PREF_ENTER_FOR_BACKTRACKING,
                		"Use Enter Key for backtracking",
                		"If enabled, the enter key sends a semicolon(';') when\n" +
                		"while the console is in 'get_single_char/1'-mode, \n" +
                		"e.g., when backtracking over the solutions to a goal.",
                		Option.FLAG,"false"), 
        		new SimpleOption(
                		PDTConsole.PREF_ENABLE_CONSOLE_VOODOO,
                		"intercept get_single_char/1 calls",
                		"When enabled, the console view will be able to detect\n" +
                		"whether the user input stream is read from via get_single_char/1" +
                		"(e.g. when backtracking through query results).\n" +
                		"In those situations, the console view will emulate the " +
                		"unbuffered behaviour of the SWI-Prolog default terminal/plwin " +
                		"interface. This works just fine in most cases, but there have been " +
                		"reports of problems when using Edinburgh-style io predicates.\n" +
                		"If you get unexpected io behaviour from " +
                		"your application, disabling this flag may help.",
                		Option.FLAG,"true"),                        		
                new SimpleOption(
                		PDTConsole.PREF_CONSOLE_HISTORY_FILE,
                		"History File",
                		"The Prolog Console uses this to save its command history.\n" +
                		"Just leave it empty if you do not want the command history to be persistent.",
                		Option.FILE,System.getProperty("user.home")+File.separator+".prolog_console_histroy"){
        			public String validate(String value) {
        				return ensureFileExists(value,"History File");
        			}
        		},
        		new SimpleOption(
						PDTConsole.PREF_TIMEOUT,
						"Connect Timeout",
						"Maximum time in milliseconds to wait for the console server to come up.",
						SimpleOption.NUMBER, "15000"),         
				new SimpleOption(
						PDTConsole.PREF_CONTEXT_TRACKERS,
						"active context trackers",
						"comma-separated list of trackers the console does follow",
						SimpleOption.STRING, ""){
        			public boolean isVisible() {
        				return false;
        			}
        		}
        };

    }

	private String ensureFileExists(String value, String label) {
		if (value == null) {
			return label + "must not be null";
		}
		if (value.length() == 0) {
			return label + " must not be empty";
		}
		File f = new File(value);
		if (!f.isAbsolute()) {
			return label + " mus be an absolute path";
		}
		if (f.isDirectory()) {
			return label + " exists, but is a directory";
		}
		if (!f.exists()) {
			try {
				if (!f.createNewFile()) {

					
				}
			} catch (IOException e) {
				Debug.report(e);
				return "could not create: "+label;				
			}
			
		}		
		if (!f.canWrite()) {
			return label + " exists, but is not writable";
		}
		return "";
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

    public void setPreferenceValue(String key,String value){
    	getPreferenceStore().setValue(key,value);
    }
    
	public void reconfigure() {
		// XXX reconnect console here.
		
	}
}
