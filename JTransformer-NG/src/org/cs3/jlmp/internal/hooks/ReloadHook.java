/*
 */
package org.cs3.jlmp.internal.hooks;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPProject;
import org.cs3.jlmp.internal.natures.JLMPProjectNature;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
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
    public void onInit(final PrologSession initSession) {
        try {
//            Job j = new Job("JLMP Reload Job") {
//                protected IStatus run(IProgressMonitor monitor) {
//                    //monitor.beginTask("reloading state",IProgressMonitor.UNKNOWN);
//                    try {
                        IProject[] projects = ResourcesPlugin.getWorkspace()
                                .getRoot().getProjects();
                        for (int i = 0; i < projects.length; i++) {
                            IProject project = projects[i];

                            if (project.isAccessible()
                                    && project.hasNature(JLMP.NATURE_ID)) {
                                JLMPProjectNature jlmpProject = (JLMPProjectNature) project
                                        .getNature(JLMP.NATURE_ID);

                                PrologInterface pif = jlmpProject
                                        .getPrologInterface();
                                /*
                                 * simply touching the cs should trigger
                                 * creation and reload if neccesary.
                                 */
                                pif.getConsultService(JLMP.SRC).setRecording(
                                        true);
                                pif.getConsultService(JLMP.EXT).setRecording(
                                        true);
                                pif.getConsultService(JLMP.EXT)
                                        .setAppendingRecords(true);
                                jlmpProject.reconfigure(initSession);
                            }
                        }

//                    } catch (Throwable e) {//well... uh.....eclipse
//                                                    // eats my
//                        // exceptions :-(
//                        Debug.report(e);
//                        return Status.CANCEL_STATUS;
//                    }finally{
//                        monitor.done();
//                    }
//                    return Status.OK_STATUS;
//                }
//            };
//            j.schedule();
//            j.join();
            

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
    public void afterInit() {
        try {
            IProgressMonitor monitor =new NullProgressMonitor();
            IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                    .getProjects();
            for (int i = 0; i < projects.length; i++) {
                final IProject project = projects[i];

                if (project.isAccessible() && project.hasNature(JLMP.NATURE_ID)) {
                    JLMPProject jlmpProject = (JLMPProject) project
                            .getNature(JLMP.NATURE_ID);
                    PrologInterface pif = jlmpProject.getPrologInterface();
                    /*
                     * finaly, we trigger full build of the project, so that any
                     * modifications that happend while while the pif was down
                     * are respected. This sounds more work than it actualy is:
                     * most of the pefs should be cached on the local
                     * filesystem.
                     */
//
//                    Job j = new Job("JLMP Builder Job") {
//                        protected IStatus run(IProgressMonitor monitor) {
//                            try {
                                //JLMPProjectNature nature =
                                // (JLMPProjectNature)
                                // p.getNature(JLMPProjectNature.NATURE_ID);
                                //nature.getBuilder().build(null,0,monitor);
                                project.build(
                                        IncrementalProjectBuilder.FULL_BUILD,
                                        monitor);
//                            } catch (Throwable e) {//well...
//                                // uh.....eclipse
//                                // eats my
//                                // exceptions :-(
//                                Debug.report(e);
//                                return Status.CANCEL_STATUS;
//                            }
//
//                            return Status.OK_STATUS;
//                        }
//                    };
//                    j.schedule();
                }
            }
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
    public void beforeShutdown(PrologSession session) {
        ;
    }

}
