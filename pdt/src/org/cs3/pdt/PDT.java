package org.cs3.pdt;

import java.util.ArrayList;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

/**
 * All kinds of string keys used by the pdt.
 */
public final  class PDT {
	
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
     * The "master-switch" for auto-consulting of files.
     * It defaults to "false". for 0.1.1 - see PDT-23 
     * If set to "true" , every prolog source file on the sourc path will
     * be automaticaly consulted as long as its PROP_NO_AUTO_CONSULT
     * flag is not set.
     */
    public static final String PREF_AUTO_CONSULT = "pdt.auto.consult";
    
    /**
     * the default to use for PROP_SOURCE_PATH
     */
    public static final String PREF_SOURCE_PATH_DEFAULT = "pdt.source.path.default";
	public static final String PLUGIN_ID = "org.cs3.pdt";

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
     * a regular expression. Only files in the source folder, that 
     * match this pattern are considered during builds.
     */
    public static final String PROP_SOURCE_INCLUSION_PATTERN = "pdt.inclusion.pattern";
	
    /**
     * a regular expression. Files in the source folder, that match thise pattern
     * are exlcluded from the build, even if they match the inclusion pattern.
     */
    public static final String PROP_SOURCE_EXCLUSION_PATTERN = "pdt.exlusion.pattern";
   
    
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
     * The basename of the resource bundle to be used by the pdt ui
     */
    public final static String RES_BUNDLE_UI = "org.cs3.pdt.ui";
    /**
     * log file location used by the pdt plugin.
     */
    public static final String PREF_CLIENT_LOG_FILE = "pdt.logfile";
	
	
	/**
     * @return all open IPrologProjects that operate on the given PrologInterface instance.
     * @throws CoreException
     * @deprecated we wil find a more general solution in PDT 0.2
     */
    public  static IPrologProject[] getPrologProjects(PrologInterface pif) throws CoreException{
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
        ArrayList l = new ArrayList();
        for (int i = 0; i < projects.length; i++) {
            IProject project = projects[i];            
            if(project.isAccessible()&&project.hasNature(PDT.NATURE_ID)){
                IPrologProject prologProject = (IPrologProject) project.getNature(PDT.NATURE_ID);
                if(prologProject.getPrologInterface()==pif){
                    l.add(prologProject);
                }
            }
        }
        IPrologProject[] r = new IPrologProject[l.size()];
        return (IPrologProject[]) l.toArray(r);
    }

}