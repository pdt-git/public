package org.cs3.pdt.core;

public class PDTCore {

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
	public final static String NATURE_ID = "org.cs3.pdt.core.PDTProjectNature";
	
	public static final String BUILDER_ID = "org.cs3.pdt.core.MetaDataBuilder";
	/**
	 * The key to which the meta data consult sevice is bound.
	 */
	public static final String CS_METADATA = "metadata";
	/**
	 * An absolute os file system path to the directory containing the prolog
	 * files of the pdt metadata engine.
	 */
	public final static String PREF_METADATA_ENGINE_DIR = "pdt.metadata.engine.dir";
	
	/**
	 * An absolute os file system path to the directory containing metadata
	 * store
	 */
	public final static String PREF_METADATA_STORE_DIR = "pdt.metadata.store.dir";
	/**
	 * The "master-switch" for auto-consulting of files.
	 * It defaults to "false". for 0.1.1 - see PDT-23 
	 * If set to "true" , every prolog source file on the sourc path will
	 * be automaticaly consulted as long as its PROP_NO_AUTO_CONSULT
	 * flag is not set.
	 */
	public static final String PREF_AUTO_CONSULT = "pdt.auto.consult";
	/**
	 * a path.separator-separated list of project relative paths pointing to the
	 * source directories of a project.
	 */
	public static final String PROP_SOURCE_PATH = "pdt.source.path";
	/**
	 * a file for which this property is "true" will NOT  be consulted each time it
	 * is touched by the meta data builder. This option only applies when 
	 * PREF_AUTO_CONSULT is set.
	 */
	public static final String PROP_NO_AUTO_CONSULT = "pdt.no.auto.consult";
	/**
	 * the default to use for PROP_SOURCE_PATH
	 */
	public static final String PREF_SOURCE_PATH_DEFAULT = "pdt.source.path.default";
	/**
	 * a regular expression. Only files in the source folder, that 
	 * match this pattern are considered during builds.
	 */
	public static final String PROP_SOURCE_INCLUSION_PATTERN = "pdt.inclusion.pattern";
	/**
	 * a regular expression. Files in the source folder, that match thise pattern
	 * are exlcluded from the build, even if they match the inclusion pattern.
	 */
	public static final String PROP_SOURCE_EXCLUSION_PATTERN = "pdt.exlusion.pattern";
	public static final String PLUGIN_ID = "org.cs3.pdt.core";

}
