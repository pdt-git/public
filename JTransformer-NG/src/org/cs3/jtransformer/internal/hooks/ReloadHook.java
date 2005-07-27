/*
 */
package org.cs3.jtransformer.internal.hooks;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.JTransformerProject;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

/**
 */
public class ReloadHook implements LifeCycleHook {

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
     */
    public void onInit(PrologInterface pif, final PrologSession initSession) {
        try {

            JTransformerProject[] jtransformerProjects = JTransformer.getJTransformerProjects(pif);
            for (int i = 0; i < jtransformerProjects.length; i++) {
                // XXX: ld: i don't like that cast. Any idee?
                JTransformerProjectNature jtransformerProject = (JTransformerProjectNature) jtransformerProjects[i];

                jtransformerProject.reconfigure(initSession);

            }
            JTransformerPlugin plugin = JTransformerPlugin.getDefault();
            String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE,
                    "false");
            if (Boolean.valueOf(v).booleanValue()) {
                plugin.reload(initSession);
            }
                //ld: see JT-147
                initSession.queryOnce("update_java_lang");
            
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
     */
    public void afterInit(PrologInterface pif) {
        try {
            IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IWorkspaceDescription wd = workspace
                    .getDescription();
            if (!wd.isAutoBuilding()) {
                return;
            }
            IProgressMonitor monitor = new NullProgressMonitor();
            IProject[] projects = workspace.getRoot()
                    .getProjects();
            final JTransformerProject[] jtransformerProjects = JTransformer.getJTransformerProjects(pif);
            Job j = new Job("Building workspace") {
                public IStatus run(IProgressMonitor monitor) {
                    try {
                        for (int i = 0; i < jtransformerProjects.length; i++) {

                            JTransformerProject jtransformerProject = jtransformerProjects[i];
                            IProject project = jtransformerProject.getProject();
                            project.build(IncrementalProjectBuilder.FULL_BUILD,
                                    monitor);

                        }
                    } catch (OperationCanceledException opc) {
                        return Status.CANCEL_STATUS;
                    } catch (Exception e) {
                        return new Status(IStatus.ERROR, JTransformer.PLUGIN_ID, -1,
                                "Problems during build", e);
                    }
                    return Status.OK_STATUS;
                }
			      public boolean belongsTo(Object family) {
				         return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
			      }
				
            };

			//j.setRule(JTransformer.JTransformer_BUILDER_SCHEDULING_RULE);
			j.setRule(workspace.getRoot());
			j.schedule();
			
        } catch (Throwable e) {
            Debug.report(e);
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
     */
    public void beforeShutdown(PrologInterface pif, PrologSession session) {
        JTransformerPlugin plugin = JTransformerPlugin.getDefault();
        String v = plugin.getPreferenceValue(JTransformer.PREF_USE_PEF_STORE, "false");
        if (Boolean.valueOf(v).booleanValue()) {
            JTransformerPlugin.getDefault().save(session);
        }
    }

}
