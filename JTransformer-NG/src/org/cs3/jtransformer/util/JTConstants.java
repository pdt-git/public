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
}
