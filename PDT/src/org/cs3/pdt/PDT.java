package org.cs3.pdt;

import org.eclipse.core.runtime.QualifiedName;

/**
 * All kinds of string keys used by the pdt.
 */
public interface PDT {

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
     * the port to use for writing fact data into the prolog system.
     */
    public static final String PREF_CONSULT_PORT = "pdt.consult.port";

    /**
     * A list of absolute os filesystem paths, separated by your favourite
     * path.separator (i.e. a colon on most unix systems)
     * 
     * The entries may be files as well as directories. Each file found in this
     * list will be consulted. For a directory, each file in the respective
     * directory will be consulted. Note that by default, subdirectories will
     * not be consulted. You can change this behaviour by associating "true"
     * with the property key CONSULT_RECURSIVE
     * 
     * The pdt will make the prolog interface consult the specified locations on
     * startup aswell as on every change to the preference value bound to this
     * property key. Note that the pdt will also reconsult any file that is
     * found using the above methods, if and when it is modified in the prolog
     * editor.
     */
    public final static String PREF_CONSULT_PATH = "pdt.consult.path";

    public final static String PREF_CONSULT_RECURSIVE = "pdt.consult.recursive";

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
     * An absolute os file system path to the directory containing the prolog
     * files of the jtransformer engine.
     */
    public final static String PREF_JTRANSFORMER_ENGINE_DIR = "jtransformer.engine.dir";

    /**
     * the absolute os file system path to the directory containing the SWI
     * Prolog installation to use. Typicaly something like pdt_HOME/swipl
     */
    public final static String PREF_SWIPL_DIR = "pdt.swipl.dir";

    /**
     * the port on which the RaPlaRPC server is listening.
     */
    public static final String PREF_SERVER_PORT = "pdt.server.port";

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
    public static final String PROP_AUTO_CONSULT = "pdt.auto.consult";

    /**
     * if set to "true", the prolog interface will not try to start the server
     * process, but instead expect it to be allready running. This option is
     * mainly usefull for debugging the server process.
     */
    public final static String PREF_SERVER_STANDALONE = "pdt.server.standalone";

    /**
     * the name of the swi prolog executable to use, e.g. <code>"xpce"</code>
     * or <code>"/usr/local/bin/xpce"</code>.
     */
    public static final String PREF_SWIPL_EXECUTABLE = "pdt.swi.executable";

    /**
     * if set to "true", the prolog interface will use a pool of PrologSessions
     * and try to reuse disposed sessions if possible. Creating a new session
     * typicaly includes the creation of a socket connection, which can be
     * rather expensive. With session pooling, a disposed Session will keep its
     * connection alive. Reusing it _should_ give some advantage over creating a
     * new one. However, since this is a relatively new feature, and not 100%
     * tested, the default is to go without pooling.
     */
    public static final String PREF_USE_SESSION_POOLING = "pdt.use.session.pooling";

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
     * contains the classpath to use when starting the server process. It
     * should, for instance, contains the class
     * org.cs3.pl.prolog.PrologInterfaceServer
     */
    public final static String PREF_SERVER_CLASSPATH = "pdt.server.classpath";

    /**
     * The basename of the resource bundle to be used by the pdt ui
     */
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";

}