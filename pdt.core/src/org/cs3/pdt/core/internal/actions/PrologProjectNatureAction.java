package org.cs3.pdt.core.internal.actions;

import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * 
 * @inheritDoc
 */
public class PrologProjectNatureAction implements IObjectActionDelegate {
	private IWorkbenchPart targetPart;

	private IAction action;

	private IProject project;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
	 *      org.eclipse.ui.IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		this.action = action;
		this.targetPart = targetPart;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	public void run(IAction action) {
		try {
			if (project == null) {
				action.setChecked(false);
				return;
			}

			IProjectDescription ipd = project.getDescription();
			if (ipd.hasNature(PDTCore.NATURE_ID)) {
				PDTCoreUtils.removePDTNature(project);
				action.setChecked(false);
			} else {
				//removeNatureFromAllOtherProjects();
				PDTCoreUtils.addPDTNature(project);
				action.setChecked(true);
			}
			action
					.setChecked(project.getDescription().hasNature(
							PDTCore.NATURE_ID));
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	

	/**
	 * @param project2
	 * @throws CoreException
	 */
	private void removeNatureFromAllOtherProjects() throws CoreException {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot()
				.getProjects();
		for (int i = 0; i < projects.length; i++)
			if (projects[i].isOpen())
				PDTCoreUtils.removePDTNature(projects[i]);
	}

	/*
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		project = null;
		if (selection instanceof IStructuredSelection) {
			Object obj = ((IStructuredSelection) selection).getFirstElement();
			if (obj instanceof IProject) {
				// the plugin.xml file should make sure it is indeed
				// a java project
				project = (IProject) obj;
			} else if (obj instanceof IAdaptable) {
				IAdaptable a = (IAdaptable) obj;
				IResource r = (IResource) a.getAdapter(IResource.class);
				if (r != null && IResource.PROJECT == r.getType()) {
					project = (IProject) r;
				}
			}			
		}
		if (project != null) {
			if (project.isOpen()) {
				action.setEnabled(true);
				try {
					action.setChecked(project.getDescription().hasNature(
							PDTCore.NATURE_ID));
				} catch (CoreException e) {
					Debug.report(e);
				}
			} else {
				action.setEnabled(false);
			}
		}
	}
}