package org.cs3.pdt.common;

public final class PDTCommon {
	
	private PDTCommon() {}
	
	/**
	 * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
	 * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
	 * 
	 * The property will be read out once the Debug class is loaded, and the
	 * debug level will be set accordingly. After that, the level can be changed
	 * using the static Debug.setDeubgLevel(int) method.
	 */
	public final static String PREF_DEBUG_LEVEL = "debug.level";
	public final static String PREF_DEBUG_OUTPUT_TO = "debug.output.to";
	
	/**
	 * log file location used by the pdt plugin.
	 */
	public static final String PREF_CLIENT_LOG_FILE_DIR = "pdt.logfile";
	
}
