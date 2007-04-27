package org.cs3.jtransformer.internal.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.jtransformer.JTDebug;
import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.util.JTUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 * Called by Eclipse to implement real-time updates and building of
 * JTransformer-Projects.
 * 
 * @inheritDoc
 */
public class JTransformerNatureAction implements IObjectActionDelegate {
	public final static String ACTION_ID = "org.cs3.pl.JTransformer.JTransformerNatureAction";

	List projects = new ArrayList();

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
	 *      org.eclipse.ui.IWorkbenchPart)
	 */
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */

	public void run(IAction action) {
		if (projects.size() == 0) {
			action.setChecked(false);
			return;
		}
		JTransformerNatureAssigner assigner = new JTransformerNatureAssigner(projects);
		int status = assigner.toggleJTransformerNatureOnAllProjects();
		if(status == JTransformerNatureAssigner.STATUS_ADDED) {
			action.setChecked(true);
		} else if(status == JTransformerNatureAssigner.STATUS_REMOVED) {
			action.setChecked(false);
		}
		
	}

	/*
	 * TODO CLEAN UP THIS METHOD! von ld, fuer ld :-) (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		projects = new ArrayList();
		if (selection instanceof IStructuredSelection) {
			IStructuredSelection selections = ((IStructuredSelection) selection);
			for (Iterator iter = selections.iterator(); iter.hasNext();) {
				Object obj = iter.next();

				// Object obj = ((IStructuredSelection)
				// selection).getFirstElement();
				ifProjectAddToList(projects, obj);
			}

		}
		action.setEnabled(false);

		if (projects.size() == 0 || !allProjectsAreOpen(projects)) {
			return;
		}
		try {
			if (projects.size() == 1) {
				action.setEnabled(true);
				boolean checked = ((IProject) projects.get(0)).getDescription()
						.hasNature(JTransformer.NATURE_ID);
				action.setChecked(checked);
			} else if (noProjectHasJTNature(projects)) {
				action.setEnabled(true);
				action.setChecked(false);
			} else if (JTUtils.allProjectsHaveJTNature(projects)) {
				action.setEnabled(true);
				action.setChecked(true);
			}
		} catch (CoreException e) {
			JTDebug.report(e);
		}

	}

	private void ifProjectAddToList(List projects, Object obj) {
		try {
			if (obj instanceof IProject
					&& ((IProject) obj).isOpen() 
					&& ((IProject) obj).getDescription().hasNature(
							JavaCore.NATURE_ID)) {
				// the plugin.xml file should make sure it is indeed
				// a java project
				projects.add(obj);

			} else if (obj instanceof IAdaptable) {
				IAdaptable a = (IAdaptable) obj;
				IResource r = (IResource) a.getAdapter(IResource.class);
				if (r != null
						&& IResource.PROJECT == r.getType()
						&& ((IProject) r).isOpen()
						&& ((IProject) r).getDescription().hasNature(
								JavaCore.NATURE_ID)) {
					projects.add(r);
				}
			}
		} catch (CoreException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private boolean allProjectsAreOpen(List projects) {
		for (Iterator iter = projects.iterator(); iter.hasNext();) {
			IProject element = (IProject) iter.next();
			if (!element.isOpen()) {
				return false;
			}
		}
		return true;
	}


	private boolean noProjectHasJTNature(List projects) throws CoreException {
		for (Iterator iter = projects.iterator(); iter.hasNext();) {
			IProject project = (IProject) iter.next();	
			if(project.getDescription().hasNature(JTransformer.NATURE_ID)) {
				return false;
			}
		}
		return true;
	}
}