/*
 */
package org.jlmp.internal.hooks;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

/**
 */
public class ReloadHook implements LifeCycleHook {

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
     */
    public void onInit(PrologSession initSession) {
        try {
            IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                    .getProjects();
            for (int i = 0; i < projects.length; i++) {
                IProject project = projects[i];

                if (project.isAccessible()
                        && project.hasNature(JLMPProjectNature.NATURE_ID)) {
                    JLMPProjectNature jlmpProject =  (JLMPProjectNature) project.getNature(JLMPProjectNature.NATURE_ID);
                    PrologInterface pif = jlmpProject.getPrologInterface();
                    /*
                     * simply touching the cs should trigger creation and reload
                     * if neccesary.
                     */
                    pif.getConsultService(JLMP.SRC);
                    pif.getConsultService(JLMP.EXT);
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
     * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
     */
    public void afterInit() {
        // TODO Auto-generated method stub

    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
     */
    public void beforeShutdown(PrologSession session) {
        // TODO Auto-generated method stub

    }

}
