
package org.cs3.jlmp;

import org.eclipse.core.resources.IProject;

/**
 * defines constants and ids that are relevant to the JLMP Plugin.
 */
public final class JLMP {

    /**
     * key that is used to identify the consultservice for external PEFs.
     */
    public static final String EXT = "ext";
    
    /**
     * key that is used to identify the consultservice for src PEFs.
     */
    public static final String SRC = "src";
    
    /**
     * the id of the JLMP Project Nature
     */
    public final static String NATURE_ID = "org.cs3.jlmp.JLMPProjectNature";
    
    /**
     * the id of the JLMP Project Builder 
     */    
    public static final String BUILDER_ID = "org.cs3.jlmp.JLMPProjectBuilder";
    
    /**
     * the id of the JLMP Plugin 
     */        
    public static final String PLUGIN_ID = "org.cs3.jlmp";
    
    /**
     * Type constant identifying the JLMP Problem marker type.
     */
    public static final String PROBLEM_MARKER_ID = "org.cs3.jlmp.jlmpproblem";
    
    /**
     * The id of the JLMP Project Listener extension point.
     */
    public static final String EP_PROJECT_LISTENER = "projectlistener";
   
    /**
     * The id of the LifeCycleHook that takes care of restoring the PEF base
     * after a restart of the PrologInterface.
     * Clients can use this hook as a dependency for their own 
     * hooks as required.
     */
    public final static String RELOAD_HOOK_ID = "ReloadHook";

    /**
     * The preference identified by this key contains the default value for the
     * per-project output source folder.
     * 
     *  value should be an absolute workspace path string to an existing folder. 
     */
    public static final String PREF_DEFAULT_OUTPUT_FOLDER = "jlmp.default.output.folder";

    /**
     * Project property containing the the output source fodler for generated source code.
     * value should be an absolute workspace path string to an existing folder.
     */
    public static final String PROP_OUTPUT_FOLDER = "jlmp.output.folder";
    
    /**
     * Project property defining wether existing source files should be modified in-place.
     * value should be either "true" or "false"
     */
    public static final String PROP_INPLACE = "jlmp.inplace";
    
    /**
     * Hook id for the LifeCycleHook that reconfigures a JLMPProject.
     * unfortunately this depends on the project name, so i cannot make it a constant.
     * @param p
     * @return the hook id
     */
    public static final String RECONGIFURE_PROJECT_HOOK(IProject p) {
        return "reconfigure."+p;
    }
}
