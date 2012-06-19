package org.cs3.pdt.runtime;

public interface PrologRuntime {

	String PLUGIN_ID = "org.cs3.pdt.runtime";

    String EP_BOOTSTRAP_CONTRIBUTION = "bootstrapContribution";

	String EP_PROLOG_LIBRARY = "prologLibrary";

    String EP_HOOKS = "hooks";


    // Preferences
    public static final String EP_TRACKERS = "prologContextTracker";
	public static final String PREF_HIDE_PLWIN = "pif.hide_plwin";
	public static final String PREF_SERVER_LOGDIR = "pif.server_log_dir";
	
	public static final String PREF_FILE_SEARCH_PATH = "pif.file_search_path";
	public static final String PREF_INVOCATION = "pif.invocation";
	public static final String PREF_EXECUTABLE = "pif.executable";
	public static final String PREF_COMMAND_LINE_ARGUMENTS = "pif.command.line.arguments";
	public static final String PREF_ENVIRONMENT = "pif.environment";
	public static final String PREF_ADDITIONAL_STARTUP = "pif.additional.startup";
	public static final String PREF_TIMEOUT = "pif.timeout";
	public static final String PREF_HOST = "pif.host";
	public static final String PREF_PORT = "pif.port";
	public static final String PREF_GENERATE_FACTBASE = "pif.genfactbase";
	public static final String PREF_META_PRED_ANALYSIS = "pif.metapred";

    
}
