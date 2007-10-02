
package org.cs3.jtransformer;

import java.util.ArrayList;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.jobs.ISchedulingRule;

/**
 * defines constants and ids that are relevant to the JTransformer Plugin.
 */
public final class JTransformer {

	/**
	 * JTransformer "core" engine.
	 */
	public static final String LIB_ENGINE = "org.cs3.jtransformer.engine";
	
	/**
	 * Error message id when the PrologInterfaceException
	 * was catched.
	 */
    public static final int ERR_PROLOG_INTERFACE_EXCEPTION = 1;
	
	/**
	 * Error context id for the JTransformer Nature (re)initialization.
	 */
    public static final int ERR_CONTEXT_NATURE_INIT = 2;

    /**
	 * Error context id for an arbitrary failed action.
	 */
    public static final int ERR_CONTEXT_ACTION_FAILED = 3;
    
    /**
     * Error occurred in the source regeneration.
     */
    public static final int ERR_CONTEXT_SOURCE_REGENERATION = 4;
    
    /**
     * Any exception occurred.
     */
    public static final int ERR_CONTEXT_EXCEPTION = 5;
    
    public static final int ERR_UNKNOWN = 6;
    
    /**
     * Errors occured while weaving.
     */
    public static final int ERR_WEAVING_FAILED = 7;

    /**
     * gen toplevels failed and raised an exception.
     */
    public static final int ERR_WRITING_WOVEN_CODE = 8;

    /**
     * key that is used to identify the consultservice for external PEFs.
     */
    public static final String EXT = "ext";
    
    /**
     * key that is used to identify the consultservice for src PEFs.
     */
    public static final String SRC = "src";
    
    /**
     * the id of the JTransformer Project Nature
     */
    public final static String NATURE_ID = "org.cs3.jtransformer.JTransformerNature";
    
    /**
     * the id of the JTransformer Project Builder 
     */    
    public static final String BUILDER_ID = "org.cs3.jtransformer.JTransformerProjectBuilder";
    
    /**
     * the id of the JTransformer Plugin 
     */        
    public static final String PLUGIN_ID = "org.cs3.jtransformer";
    
    /**
     * Type constant identifying the JTransformer Problem marker type.
     */
    public static final String PROBLEM_MARKER_ID = "org.cs3.jtransformer.jtransformerproblem";
    /**
     * The id of the JTransformer Project Listener extension point.
     */
    public static final String EP_PROJECT_LISTENER = "projectlistener";
   
    /**
     * The id of the LifeCycleHook that takes care of restoring the PEF base
     * after a restart of the PrologInterface.
     * Clients can use this hook as a dependency for their own 
     * hooks as required.
     */
    public final static String RELOAD_HOOK_ID = "ReloadHook";
    
    
    public final static String ADVICE_WEAVING_MARKER = "org.cs3.LogicAJ.advice.marker";
//    public final static String ADVICE_WEAVING_MARKER = "org.cs3.logicaj.advice.weaving.marker";

//    /**
//     * The preference identified by this key contains the default value for the
//     * per-project output source folder.
//     * 
//     *  value should be an absolute workspace path string to an existing folder. 
//     */
//    public static final String PREF_DEFAULT_OUTPUT_FOLDER = "jtransformer.default.output.folder";

    /**
     * Project property containing the the output source fodler for generated source code.
     * value should be an absolute workspace path string to an existing folder.
     */
    public static final String PROP_OUTPUT_FOLDER = "jtransformer.output.folder";
    
    /**
     * Project property defining wether existing source files should be modified in-place.
     * value should be either "true" or "false"
     */
    public static final String PROP_INPLACE = "jtransformer.inplace";

    /**
     * absolute os-filesystem path to at file where PEFs for a project
     * should be stored. 
     */
    public static final String PROP_PEF_STORE_FILE = "jtransformer.pef.store.file";
    
    /**
     * global default for PROP_PEF_STORE_FILE
     */
    public static final String PREF_DEFAULT_PEF_STORE_FILE = "jtransformer.default.pef.store.file";

    public static final String PROP_OUTPUT_PROJECT = "jtransformer.output.project";
 //   public static final String PREF_DEFAULT_OUTPUT_PROJECT = "jtransformer.default.output.project";
    
    public static final String PREF_REVERSE_INDEX = "jtransformer.reverse.indexes";

    public static final String PROP_LAST_BUILD = "jtransformer.last.build";

    public static final String PREF_USE_PEF_STORE = "jtransformer.use.pef.store";

    public static final String PROLOG_RUNTIME_KEY = "project.prolog.runtime.key";

    public static final String FACTBASE_STATE_KEY = "factbase.state";

    public static final String FACTBASE_STATE_DISABLED = "factbase.state.disabled";

    /**
     * JTransformer nature is assigned, but the facts are not generated, yet.
     */
    public static final String FACTBASE_STATE_ACTIVATED = "factbase.state.activated";

    /**
     * JTransformer nature is assigned and the fact generation is in progress.
     */
    public static final String FACTBASE_STATE_IN_PROCESS = "factbase.state.in.process";
    
    /**
     * Facts are generated.
     */
    public static final String FACTBASE_STATE_READY = "factbase.state.ready";

    /**
     * @return all open JTransformerProjects that operate on the given PrologInterface instance.
     * @throws CoreException
     * @deprecated we wil find a more general solution in PDT 0.2
     */
    public  static JTransformerProject[] getJTransformerProjects(PrologInterface pif) throws CoreException{
        IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
        ArrayList l = new ArrayList();
        for (int i = 0; i < projects.length; i++) {
            IProject project = projects[i];            
            if(project.isAccessible()&&project.hasNature(JTransformer.NATURE_ID)){
                JTransformerProject jtransformerProject = JTransformerPlugin.getNature( project);
                if(jtransformerProject.getPrologInterface()==pif){
                    l.add(jtransformerProject);
                }
            }
        }
        JTransformerProject[] r = new JTransformerProject[l.size()];
        return (JTransformerProject[]) l.toArray(r);
    }
//  FIXME XXX TODO Warning, warning, warning!!!
// LogicAJ relies on this rule, it should use the workspaceroot instead	
//
/**
 * This only exists temporarily so LogicAJ does not break.
 * @deprecated use ResourcesPlugin.getWorkspace().getRoot() instead.
 */	
public static final ISchedulingRule JTransformer_BUILDER_SCHEDULING_RULE = new ProxyRule();

public static final String PREF_CLIENT_LOG_FILE = "jtransformer.logfile";

public static final String PREF_DEBUG_LEVEL = "jtransformer.debug.level";

public static final String SUBSCRIPTION_PREFIX = "JTransformerSubscription_";

public static final String PROP_REWRITE_PROJECT_FILES_WHEN_WEAVING = "jtransformer.rewrite.project.files";

/**
 * the idea is to keep the classloader from instantiating the resources plugin.
 * 
 * @deprecated only used as long as JTransformer_BUILDER_SCHEDULING_RULE is used.
 */
static class ProxyRule implements ISchedulingRule{
	ISchedulingRule target=null;
	public ISchedulingRule getTarget() {
		if(target==null){
			target= ResourcesPlugin.getWorkspace().getRoot();
		}
		return target;
	}
	public boolean contains(ISchedulingRule rule) {
		return getTarget().contains(rule);
	}
	public boolean isConflicting(ISchedulingRule rule) {
		return getTarget().isConflicting(rule);
	}
	public boolean equals(Object obj) {	
		return getTarget().equals(obj);
	}
	public int hashCode() {	
		return getTarget().hashCode();
	}
	
}
//
//	static class MutexRule implements ISchedulingRule {
//	      public boolean isConflicting(ISchedulingRule rule) {
//	         return rule == this;
//	      }
//	      public boolean contains(ISchedulingRule rule) {
//	         return rule == this || rule == ResourcesPlugin.getWorkspace().getRoot() ||
//			 ResourcesPlugin.getWorkspace().getRoot().contains(rule);
//	      }
//	}

}
