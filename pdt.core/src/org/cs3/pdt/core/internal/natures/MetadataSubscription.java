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

import java.util.Map;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pdt.runtime.PrologLibraryManager;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
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

	public MetadataSubscription(String projectName, String id,String pifId) {

		super(id, pifId,
				"used to store and process meta information on prolog"
						+ "source code found in project " + projectName,
				projectName + " - metadata", PDTCore.PLUGIN_ID, true);
		setProjectName(projectName);
	}

	public void configure(PrologInterface pif) {
		pif.addLifeCycleHook(this, getId(), new String[0]);
		if (!pif.isDown()) {
			afterInit(pif);
		}
	}

	public void deconfigure(PrologInterface pif) {
		pif.removeLifeCycleHook(getId());
		if (pif.isUp()) {
			PrologSession session = pif.getSession();
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

	public Map saveState() {
		Map map = super.saveState();
		map.put("project", getProjectName());
		return map;
	}

	private String getProjectName() {
		return this.projectName;
	}

	public void onInit(PrologInterface pif, PrologSession initSession) {
		;

	}

	public void afterInit(PrologInterface pif) {
		PrologLibraryManager mgr = PrologRuntimePlugin.getDefault().getLibraryManager();
		PrologSession s = pif.getSession();
		try{
			PLUtil.configureFileSearchPath(mgr,s,new String[]{PDTCore.ENGINE_ID});
			Map map = s.queryOnce(
					"use_module(library('/org/cs3/pdt/annotate/pdt_annotator'))," +
					"use_module(library('/org/cs3/pdt/core/pdt_meta_info'))," +
					"use_module(library('/org/cs3/pdt/metadata/pdtplugin'))," +
					"use_module(library(pif_observe))," +
					"use_module(library('/org/cs3/pdt/metadata/abba_graph_generator'))");
			if(map==null){
				throw new RuntimeException("could not load annotator framework");
			}
			map=null;
			map = s.queryOnce(
					"register_annotator(library('/org/cs3/pdt/annotate/op_annotator'))," +
					"register_annotator(library('/org/cs3/pdt/annotate/fileref_annotator'))," +
					"register_annotator(library('/org/cs3/pdt/annotate/export_annotator'))," +
					"register_annotator(library('/org/cs3/pdt/annotate/member_annotator'))," +
					"register_annotator(library('/org/cs3/pdt/annotate/indexer'))");
			if(map==null){
				throw new RuntimeException("could not load annotator modules: query failed.");
			}
		}catch(Throwable t){
			Debug.rethrow(t);
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
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

	private IStatus runBuildJob(IProgressMonitor monitor) {
		try {
			Debug.debug("runBuildJob: job started");

			IProject project = getProject();
			Debug.debug("lets build project " + project);
			project.build(IncrementalProjectBuilder.FULL_BUILD,
					PDTCore.METADATA_BUILDER_ID, null, monitor);
			project.build(IncrementalProjectBuilder.FULL_BUILD,
					PDTCore.PROLOG_BUILDER_ID, null, monitor);
			Debug
					.debug("runBuildJob: done: "
							+ project);

		} catch (OperationCanceledException opc) {
			return Status.CANCEL_STATUS;
		} catch (Exception e) {
			return new Status(IStatus.ERROR, PDTCore.PLUGIN_ID, -1,
					"Problems during build", e);
		}
		return Status.OK_STATUS;
	}
}
