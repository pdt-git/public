/*
 */
package org.cs3.jlmp.internal.hooks;

import java.io.File;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.jlmp.JLMPProject;
import org.cs3.jlmp.internal.natures.JLMPProjectNature;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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
    public void onInit(PrologInterface pif, final PrologSession initSession) {
        try {

            JLMPProject[] jlmpProjects = JLMP.getJLMPProjects(pif);
            for (int i = 0; i < jlmpProjects.length; i++) {
                // XXX: ld: i don't like that cast. Any idee?
                JLMPProjectNature jlmpProject = (JLMPProjectNature) jlmpProjects[i];

                jlmpProject.reconfigure(initSession);

            }
            JLMPPlugin plugin = JLMPPlugin.getDefault();
            String v = plugin.getPreferenceValue(JLMP.PREF_USE_PEF_STORE,
                    "false");
            if (Boolean.valueOf(v).booleanValue()) {
                plugin.reload(initSession);
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
    public void afterInit(PrologInterface pif) {
        try {
            IWorkspaceDescription wd = ResourcesPlugin.getWorkspace()
                    .getDescription();
            if (! wd.isAutoBuilding()) {
                return;
            }
            IProgressMonitor monitor = new NullProgressMonitor();
            IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
                    .getProjects();
            JLMPProject[] jlmpProjects = JLMP.getJLMPProjects(pif);
            for (int i = 0; i < jlmpProjects.length; i++) {
                // XXX: ld: i don't like that cast. Any idee?
                JLMPProjectNature jlmpProject = (JLMPProjectNature) jlmpProjects[i];
                IProject project = jlmpProject.getProject();
                project.build(IncrementalProjectBuilder.FULL_BUILD, monitor);

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
    public void beforeShutdown(PrologInterface pif, PrologSession session) {
        JLMPPlugin plugin = JLMPPlugin.getDefault();
        String v = plugin.getPreferenceValue(JLMP.PREF_USE_PEF_STORE, "false");
        if (Boolean.valueOf(v).booleanValue()) {
            JLMPPlugin.getDefault().save(session);
        }
    }

}
