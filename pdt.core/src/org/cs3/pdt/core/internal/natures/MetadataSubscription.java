/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.core.internal.natures;

import java.io.File;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceDescription;
import org.eclipse.core.resources.IncrementalProjectBuilder;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;

public class MetadataSubscription extends DefaultSubscription implements
		LifeCycleHook {

	private String projectName;

	private IProject project;

	public MetadataSubscription() {
		super();
	}

	@Override
	public boolean isVisible() {
		return false;
	}

	public MetadataSubscription(String projectName, String id, String pifId) {

		super(id, pifId, "used to store and process meta information on prolog"
				+ "source code found in project " + projectName, projectName
				+ " - metadata", PDTCore.PLUGIN_ID, true);
		setProjectName(projectName);
	}

	public void configure(PrologInterface pif) {
		pif.addLifeCycleHook(this, getId(), new String[0]);
		if (pif.isUp()) {
			PrologSession session = null;
			try {
				session = pif.getSession();
				onInit(pif, session);
			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			} finally {
				if (session != null) {
					session.dispose();
				}
			}

			afterInit(pif);
		}
	}

	public void deconfigure(PrologInterface pif) {
		pif.removeLifeCycleHook(getId());
		if (pif.isUp()) {
			PrologSession session = null;
			try {
				session = pif.getSession();
			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			}
			try {
				beforeShutdown(pif, session);
			} finally {
				if (session != null) {
					session.dispose();
				}
			}
		}
	}

	public void restoreState(Map params) {
		super.restoreState(params);
		setProjectName((String) params.get("project"));
	}

	private void setProjectName(String pname) {
		this.projectName = pname;

	}

	public Map<String, String> saveState() {
		Map<String, String> map = super.saveState();
		map.put("project", getProjectName());
		return map;
	}

	private String getProjectName() {
		return this.projectName;
	}

	public void onInit(PrologInterface pif, PrologSession initSession)
			throws PrologInterfaceException {
		/* load pdt backend facade */
		PrologLibraryManager mgr = PrologRuntimePlugin.getDefault()
				.getLibraryManager();
		PLUtil.configureFileSearchPath(mgr, initSession,
				new String[] { PDTCore.ENGINE_ID });
		initSession.queryOnce("ensure_loaded(library('facade/pdt_facade'))");

		/* setup project source paths */

		try {
			IPrologProject project = getPrologProject();
			project.updateBuildPath(initSession);
		} catch (CoreException e) {
			Debug.rethrow(e);
		}
	}

	public void afterInit(PrologInterface pif) {

		scheduleInitialBuild(pif);
	}

	public void beforeShutdown(PrologInterface pif, PrologSession session) {
		;

	}

	private void scheduleInitialBuild(PrologInterface pif) {
		try {
			IWorkspace workspace = ResourcesPlugin.getWorkspace();
			IWorkspaceDescription wd = workspace.getDescription();
			if (!wd.isAutoBuilding()) {
				return;
			}
			Debug.debug("autobuild is on, let's rock.");

			Job j = new Job("Building Prolog Metadata") {
				public IStatus run(IProgressMonitor monitor) {
					return runBuildJob(monitor);
				}

				public boolean belongsTo(Object family) {
					return family == ResourcesPlugin.FAMILY_MANUAL_BUILD;
				}

			};

			j.setRule(workspace.getRoot());
			j.schedule();
			Debug.debug("triggerInitialBuild: scheduling buildjob");
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	private IProject getProject() {
		if (project == null) {
			project = ResourcesPlugin.getWorkspace().getRoot().getProject(
					getProjectName());
		}
		return project;
	}

	private IPrologProject getPrologProject() throws CoreException {
		IProject project = getProject();
		return PDTCoreUtils.getPrologProject(project);

	}

	private IStatus runBuildJob(IProgressMonitor monitor) {
		try {
			Debug.debug("runBuildJob: job started");

			IProject project = getProject();
			Debug.debug("lets build project " + project);
			// project.build(IncrementalProjectBuilder.FULL_BUILD,
			// PDTCore.METADATA_BUILDER_ID, null, monitor);
			project.build(IncrementalProjectBuilder.FULL_BUILD,
					PDTCore.PROLOG_BUILDER_ID, null, monitor);
			Debug.debug("runBuildJob: done: " + project);

		} catch (OperationCanceledException opc) {
			return Status.CANCEL_STATUS;
		} catch (Exception e) {
			return new Status(IStatus.ERROR, PDTCore.PLUGIN_ID, -1,
					"Problems during build", e);
		}
		return Status.OK_STATUS;
	}
}
