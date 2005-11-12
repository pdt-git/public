package org.cs3.pdt.runtime;

public class PrologRuntime {
	
	public static final String PLUGIN_ID = "org.cs3.pdt.runtime";
	public static final String LOC_PIF = "engine/PrologInterface";
	
	/**
     * The fully-qualified classname of a class that extends
     * PrologInterfaceFactory.
     */
    public final static String PREF_PIF_IMPLEMENTATION = "pdt.pif.implementation";
    public static final String EP_HOOKS = "hooks";
    public static final String EP_BOOTSTRAP_CONTRIBUTION = "bootstrapContribution";
	public static final String EP_TRACKERS = "prologContextTracker";
	public static final String EP_PROLOG_LIBRARY = "prologLibrary";
    

}
