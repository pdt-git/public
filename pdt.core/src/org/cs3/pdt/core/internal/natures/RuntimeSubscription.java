package org.cs3.pdt.core.internal.natures;

import java.util.Map;

import org.cs3.pdt.core.IPrologProject;
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
				projectName + " - runtime", PDTCore.PLUGIN_ID, true);
		setProjectName(projectName);
	}

	public void configure(PrologInterface pif) {
		pif.addLifeCycleHook(this, getId(), new String[0]);
		if (pif.isUp()) {
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
		PrologLibraryManager mgr = PrologRuntimePlugin.getDefault()
				.getLibraryManager();
		IPrologProject plp = getPrologProject();
		String[] keys = plp.getPrologLibraryKeys();
		
		
		PrologSession s = pif.getSession();
		try {
			PLUtil.configureFileSearchPath(mgr, s,keys);
			
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

}
