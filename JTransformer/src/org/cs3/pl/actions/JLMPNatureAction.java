package org.cs3.pl.actions;
import org.cs3.pl.Debug;
import org.cs3.pl.natures.JLMPProjectNature;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
/**
 * Called by Eclipse to implement real-time updates and building 
 * of JLMP-Projects.
 * 
 * @inheritDoc
 */
public class JLMPNatureAction implements IObjectActionDelegate {
	private final static String ACTION_ID = "org.cs3.pl.JTransformer.JLMPNatureAction";
	private IWorkbenchPart targetPart;
	private IAction action;
	private IProject project;
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
	 *           org.eclipse.ui.IWorkbenchPart)
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
		if (project == null){
			action.setChecked(false);
			return;
		}
		try {
			IProjectDescription ipd = project.getDescription();
			String[] oldNIDs = ipd.getNatureIds();
			String[] newNIDs = null;
			if (ipd.hasNature(JLMPProjectNature.NATURE_ID)) {
				newNIDs = new String[oldNIDs.length - 1];
				int j = 0;
				for (int i = 0; i < newNIDs.length; i++) {
					if (oldNIDs[j].equals(JLMPProjectNature.NATURE_ID))
						j++;
					newNIDs[i] = oldNIDs[j];
					j++;
				}
			} else {
				newNIDs = new String[oldNIDs.length + 1];
				newNIDs[0] = JLMPProjectNature.NATURE_ID;
				System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
			}
			ipd.setNatureIds(newNIDs);
			project.setDescription(ipd, null);//TODO: add real
											  // ProgressMonitor
											  // here!
			action.setChecked(project.getDescription().hasNature(
					JLMPProjectNature.NATURE_ID));
		} catch (CoreException e) {
			Debug.report(e);
		}
	}
	
	/* TODO CLEAN UP THIS METHOD! von ld, fuer ld :-)
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *           org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			Object obj = ((IStructuredSelection) selection).getFirstElement();
			if (obj instanceof IProject) {
				// the plugin.xml file should make sure it is indeed
				// a java project
				project = (IProject) obj;
				if (project.isOpen()) {
					action.setEnabled(true);
					try {
						action.setChecked(project.getDescription().hasNature(
								JLMPProjectNature.NATURE_ID));
					} catch (CoreException e) {
						Debug.report(e);
					}
				}
				else{
					action.setEnabled(false);
				}
			
			} 
			else if (obj instanceof IJavaProject) {
				IJavaProject javaProject = (IJavaProject)obj;
				project = javaProject.getProject();
				//TODO: FIXME: the following is exactly the case above.
				// this should be cleaned up. (ld)
				if (project.isOpen()) {
					action.setEnabled(true);
					try {
						action.setChecked(project.getDescription().hasNature(
								JLMPProjectNature.NATURE_ID));
					} catch (CoreException e) {
						Debug.report(e);
					}
				}
				else{
					action.setEnabled(false);
				}
			
			} 
			else {
				project = null;
			}
		} else {
			project = null;
		}
	}
}