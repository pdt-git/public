package org.cs3.pl.common;
/**
 * This class provides constant property keys for all the System properies used
 * by JTransformer and adjacent Projects
 */
public class Properties {
	/**
	 * the absolute os file system path to the directory containing the
	 * jtransformer prolog engine. Typicaly something like
	 * JTRANSFORMER_HOME/engine
	 */
	public final static String ENGINE_DIR = "jtransformer.engine.dir";

	/**
	 * the absolute os file system path to the directory containing the SWI
	 * Prolog installation to use. Typicaly something like
	 * JTRANSFORMER_HOME/swipl
	 */
	public final static String SWIPL_DIR = "jtransformer.swipl.dir";

	/**
	 * the port on which the RaPlaRPC server is listening.
	 */
	public static final String SERVER_PORT = "jtransformer.server.port";
	
	/**
	 * the port on which the prolog console server is listening.
	 */
	public static final String CONSOLE_PORT = "jtransformer.server.port";

	/**
	 * if set to "true", the prolog interface will not try to start the server
	 * process, but instead expect it to be allready running. This option is
	 * mainly usefull for debugging the server process.
	 */
	public final static String SERVER_STANDALONE = "jtransformer.server.standalone";

//	/**
//	 * a comma-separated list of fully qualified classnames. When the
//	 * PrologInterface class is initialy loaded, it will try to load each of the
//	 * specified classes (class-for-name) thereby triggering any static blocks
//	 * within those classes. By putting them in there, you can make sure your
//	 * init/startup hooks are registered on time.
//	 */
//	public static final String PLIF_HOOKS = "jtransformer.plif.hooks";

	/**
	 * if set to "true", the prolog interface will use a pool of PrologSessions
	 * and try to reuse disposed sessions if possible. Creating a new session
	 * typicaly includes the creation of a socket connection, which can be
	 * rather expensive. With session pooling, a disposed Session will keep its
	 * connection alive. Reusing it _should_ give some advantage over creating a
	 * new one. However, since this is a relatively new feature, and not 100%
	 * tested, the default is to go without pooling.
	 */
	public static final String USE_SESSION_POOLING = "jtransformer.use.session.pooling";

	/**
	 * Specifies the default level of verbosity. Valid values are "DEBUG" (VERY
	 * verbose), "INFO", "WARNING","ERROR" and "NONE" (quiet)
	 * 
	 * The property will be read out once the Debug class is loaded, and the
	 * debug level will be set accordingly. After that, the level can be changed
	 * using the static Debug.setDeubgLevel(int) method.
	 */
	public final static String DEBUG_LEVEL = "jtransformer.debug.level";

	/**
	 * contains the classpath to use when starting the server process. It
	 * should, for instance, contains the class
	 * org.cs3.pl.prolog.PrologInterfaceServer
	 */
	public final static String SERVER_CLASSPATH = "jtransformer.server.classpath";
}