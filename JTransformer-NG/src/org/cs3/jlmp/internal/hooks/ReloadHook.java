/*
 */
package org.cs3.jlmp.internal.hooks;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.PrintStream;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

/**
 */
public class ReloadHook implements LifeCycleHook {
	public final static String HOOK_ID = "ReloadHook";

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

				if (project.isAccessible() && project.hasNature(JLMP.NATURE_ID)) {
					JLMPProjectNature jlmpProject = (JLMPProjectNature) project
							.getNature(JLMP.NATURE_ID);
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
		try {
			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
					.getProjects();
			for (int i = 0; i < projects.length; i++) {
				IProject project = projects[i];

				if (project.isAccessible() && project.hasNature(JLMP.NATURE_ID)) {
					JLMPProjectNature jlmpProject = (JLMPProjectNature) project
							.getNature(JLMP.NATURE_ID);
					PrologInterface pif = jlmpProject.getPrologInterface();
					/*
					 * finaly, we trigger an incremental build of the project,
					 * so that any modifications that happend while while the
					 * pif was down are respected.
					 */
					project.build(IncrementalProjectBuilder.FULL_BUILD, null);
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
