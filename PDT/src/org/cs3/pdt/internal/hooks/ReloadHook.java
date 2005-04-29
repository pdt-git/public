/*
 */
package org.cs3.pdt.internal.hooks;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
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
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
     */
    public void afterInit(PrologInterface pif) {
        try {
            IWorkspaceDescription wd = ResourcesPlugin.getWorkspace()
                    .getDescription();
            if (!wd.isAutoBuilding()) {
                return;
            }
            IProgressMonitor monitor = new NullProgressMonitor();
            final IPrologProject[] jlmpProjects = PDT.getPrologProjects(pif);
            Job j = new Job("Building Prolog Metadata") {
                public IStatus run(IProgressMonitor monitor) {
                    try {
                        for (int i = 0; i < jlmpProjects.length; i++) {

                            IPrologProject jlmpProject = jlmpProjects[i];
                            IProject project = jlmpProject.getProject();
                            project.build(IncrementalProjectBuilder.FULL_BUILD,
									PDT.BUILDER_ID,
									null,
                                    monitor);

                        }
                    } catch (OperationCanceledException opc) {
                        return Status.CANCEL_STATUS;
                    } catch (Exception e) {
                        return new Status(IStatus.ERROR, PDT.PLUGIN_ID, -1,
                                "Problems during build", e);
                    }
                    return Status.OK_STATUS;
                }
			      public boolean belongsTo(Object family) {
				         return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
			      }
				
            };

			
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
    }

}
