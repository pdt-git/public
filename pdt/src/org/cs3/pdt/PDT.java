package org.cs3.pdt;



/**
 * All kinds of string keys used by the pdt.
 */
public final  class PDT {
	
    public static final String PLUGIN_ID = "org.cs3.pdt";

    /**
     * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
     * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
     * 
     * The property will be read out once the Debug class is loaded, and the
     * debug level will be set accordingly. After that, the level can be changed
     * using the static Debug.setDeubgLevel(int) method.
     */
    public final static String PREF_DEBUG_LEVEL = "debug.level";
    public static final String PREF_ADD_NATURE_ON_OPEN = "pdt.ask.add_nature";
    
     
    /**
     * The basename of the resource bundle to be used by the pdt ui
     */
    
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
    /**
     * log file location used by the pdt plugin.
     */
    public static final String PREF_CLIENT_LOG_FILE = "pdt.logfile";

	/**
	 * ui scope used for keybindings etc in the prolog editor.
	 */
    public static final String CONTEXT_EDITING_PROLOG_CODE = "org.cs3.pdt.editingProlog";

	public static final String PREF_SWITCH_TO_DEFAULT_PIF = "pdt.ask.switch_to_default_pif";

	public static final String PL_PARTITIONER = "pdt.pl_partitioner";

	

}