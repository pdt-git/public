
package org.cs3.jlmp;

import java.util.ArrayList;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

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
     * absolute os-filesystem path to at file where PEFs for a project
     * should be stored. 
     */
    public static final String PROP_PEF_STORE_FILE = "jlmp.pef.store.file";
    
    /**
     * global default for PROP_PEF_STORE_FILE
     */
    public static final String PREF_DEFAULT_PEF_STORE_FILE = "jlmp.default.pef.store.file";

    public static final String PROP_OUTPUT_PROJECT = "jlmp.output.project";
    public static final String PREF_DEFAULT_OUTPUT_PROJECT = "jlmp.default.output.project";
    
    public static final String PROP_LAST_BUILD = "jlmp.last.build";

    public static final String PREF_USE_PEF_STORE = "jlmp.use.pef.store";
    
    /**
     * @return all open JLMPProjects that operate on the given PrologInterface instance.
     * @throws CoreException
     * @deprecated we wil find a more general solution in PDT 0.2
     */
    public  static JLMPProject[] getJLMPProjects(PrologInterface pif) throws CoreException{
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
        ArrayList l = new ArrayList();
        for (int i = 0; i < projects.length; i++) {
            IProject project = projects[i];            
            if(project.isAccessible()&&project.hasNature(JLMP.NATURE_ID)){
                JLMPProject jlmpProject = (JLMPProject) project.getNature(JLMP.NATURE_ID);
                if(jlmpProject.getPrologInterface()==pif){
                    l.add(jlmpProject);
                }
            }
        }
        JLMPProject[] r = new JLMPProject[l.size()];
        return (JLMPProject[]) l.toArray(r);
    }
	
	public static final ISchedulingRule JLMP_BUILDER_SCHEDULING_RULE = new MutexRule();

	static class MutexRule implements ISchedulingRule {
	      public boolean isConflicting(ISchedulingRule rule) {
	         return rule == this;
	      }
	      public boolean contains(ISchedulingRule rule) {
	         return rule == this || rule == ResourcesPlugin.getWorkspace().getRoot() ||
			 ResourcesPlugin.getWorkspace().getRoot().contains(rule);
	      }
	}

}
