package org.cs3.pdt;

/**
 * All kinds of string keys used by the pdt.
 */
public interface PDT {

    public static final String EP_HOOKS = "hooks";
    public static final String EP_BOOTSTRAP_CONTRIBUTION = "bootstrapContribution";
    
    /**
     * The prolog project nature.
     * <p>
     * To get an <code>IPrologProject</code> from an <code>IProject</code>
     * you would do the following:
     * <p>
     * <code>
     * IProject project = <i>(---some prolog project---)</i>;
     * <p>
     * IPrologProject prologProject = (IPrologProject)project.getNature(PDT.NATURE_ID);
     * </code>
     */
    public final static String NATURE_ID = "org.cs3.pdt.PDTProjectNature";

    /**
     * An absolute os file system path to the directory containing the prolog
     * files of the pdt metadata engine.
     */
    public final static String PREF_METADATA_ENGINE_DIR = "pdt.metadata.engine.dir";

    public static final String BUILDER_ID = "org.cs3.pdt.MetaDataBuilder";

    /**
     * An absolute os file system path to the directory containing metadata
     * store
     */
    public final static String PREF_METADATA_STORE_DIR = "pdt.metadata.store.dir";

 
    /**
     * the port on which the prolog console server is listening.
     */
    public static final String PREF_CONSOLE_PORT = "pdt.console.port";

    /**
     * the default to use for PROP_SOURCE_PATH
     */
    public static final String PREF_SOURCE_PATH_DEFAULT = "pdt.source.path.default";

    /**
     * a path.separator-separated list of project relative paths pointing to the
     * source directories of a project.
     */
    public static final String PROP_SOURCE_PATH = "pdt.source.path";

    /**
     * a file for which this property is "true" will be consulted each time it
     * is touched by the meta data builder.
     */
    public static final String PROP_NO_AUTO_CONSULT = "pdt.no.auto.consult";

   
    public static final String LOC_PIF = "engine/PrologInterface";
    public static final String LOC_MODEL = "engine/MetaData";
    public static final String LOC_ENGINE = "engine";

   
    
    /**
     * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
     * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
     * 
     * The property will be read out once the Debug class is loaded, and the
     * debug level will be set accordingly. After that, the level can be changed
     * using the static Debug.setDeubgLevel(int) method.
     */
    public final static String PREF_DEBUG_LEVEL = "debug.level";

    /**
     * The key to which the meta data consult sevice is bound.
     */
    public static final String CS_METADATA = "metadata";

    /**
     * the key to which the default workspace consult service is bound.
     */
    public static final String CS_WORKSPACE = "workspace";

   
    /**
     * The fully-qualified classname of a class that extends
     * PrologInterfaceFactory.
     */
    public final static String PREF_PIF_IMPLEMENTATION = "pdt.pif.implementation";

    /**
     * The basename of the resource bundle to be used by the pdt ui
     */
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
    /**
     * log file location used by the pdt plugin.
     */
    public static final String PREF_CLIENT_LOG_FILE = "pdt.logfile";

    

}