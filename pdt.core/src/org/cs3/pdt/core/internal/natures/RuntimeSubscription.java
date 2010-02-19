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

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.DefaultSubscription;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.FileSearchPathConfigurator;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;

public class RuntimeSubscription extends DefaultSubscription implements
		LifeCycleHook {
	private String projectName;
	private IProject project;
	private IPrologProject prologProject;
	
	public RuntimeSubscription() {
		super();
	}

	public RuntimeSubscription(String projectName, String id, String pifId) {

		super(id, pifId, "used as default runtime for project" + projectName,
				projectName + " - PDT", PDTCore.PLUGIN_ID, true);
		setProjectName(projectName);
	}

	public void configure(PrologInterface pif) {
		pif.addLifeCycleHook(this, getId(), new String[0]);
		if (pif.isUp()) {
			try {
				afterInit(pif);
			} catch (PrologInterfaceException e) {
				;
			}
		}
	}

	public void deconfigure(PrologInterface pif) {
		pif.removeLifeCycleHook(getId());
		if (pif.isUp()) {
			PrologSession session=null;
			try {
				session = pif.getSession(PrologInterface.NONE);
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

	public void restoreState(Map<String, String> params) {
		super.restoreState(params);
		setProjectName(params.get("project"));
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

	public void onInit(PrologInterface pif, PrologSession initSession) {
		;
	}

	public void afterInit(PrologInterface pif) throws PrologInterfaceException {
		PrologLibraryManager mgr = PrologRuntimePlugin.getDefault().getLibraryManager();
		IPrologProject plp = getPrologProject();
		String[] keys = null;
		try {
			keys = plp.getPrologLibraryKeys();
		} catch (CoreException e) {
			Debug.rethrow(e);
		}
		PrologSession s =null;
		try {
			s= pif.getSession(PrologInterface.NONE);
			FileSearchPathConfigurator.configureFileSearchPath(mgr, s,keys);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

	public void beforeShutdown(PrologInterface pif, PrologSession session) {
		;
	}

	private IProject getProject() {
		if (project == null) {
			project = ResourcesPlugin.getWorkspace().getRoot().getProject(
					getProjectName());
		}
		return project;
	}

	private IPrologProject getPrologProject() {

		if (prologProject == null) {
			IProject p = getProject();
			try {
				if (p == null) {
					throw new RuntimeException(
					"project is null????");
				}
				if(!p.hasNature(PDTCore.NATURE_ID)){
					throw new RuntimeException(
					"project is not a prolog project????");	
				}

				prologProject = (IPrologProject) p.getNature(PDTCore.NATURE_ID);
			} catch (CoreException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
		return prologProject;
	}

	@Override
	public void lateInit(PrologInterface pif) {
		;
	}

	@Override
	public void onError(PrologInterface pif) {
		;
	}

	@Override
	public void setData(Object data) {
		;
	}
}
