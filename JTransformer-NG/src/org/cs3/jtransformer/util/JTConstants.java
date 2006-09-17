package org.cs3.jtransformer.util;

/**
 * 
 * @author Mark Schmatz
 *
 */
public interface JTConstants
{
	/**
	 * If <tt>true</tt> then the same output directory is used as the adapted project
	 * resides in; if <tt>false</tt> then a seperate output project resp. directory
	 * is used (e.g. LogicAJOutput).<br><br>
	 * 
	 * DEFAULT: <tt>true</tt>
	 */
	public static final String SYSTEM_PROPERTY_USE_SAME_OUTDIR_WITH_SUFFIX = "org.cs3.logicaj.usesameoutdir";
	
	/**
	 * The suffix which will be added to the project name for the adapted
	 * version of the project.
	 */
	public static final String OUTPUT_PROJECT_NAME_SUFFIX = "-output";

	public static final String CTNAME_FILENAME_SEPARATOR = " ##--## ";

	public static final String RESOURCES_FILELISTS_PACKAGE = "resources.filelists";

	public static final String BUNDLE_MANIFEST_FILE = "/bundle.manifest";

	public static final String COMPLETE_RESOURCES_FILELISTS_FOLDER = "/src/resources/filelists/";

	public static final String FQCN_LIST_FILENAME = "fqcn.list";

	public static final String CT_LIST_FILENAME = "ct.list";

	public static final String DOT_PROJECT_FILE = "/.project";

	public static final String DOT_CLASSPATH_FILE = "/.classpath";

	public static final String BUNDLE_PACK_FILE = "/.bundle-pack";
	
	public static final String RESOURCES_FOLDER = "resources";
}
