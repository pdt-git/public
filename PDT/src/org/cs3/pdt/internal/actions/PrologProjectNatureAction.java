package org.cs3.pdt.internal.actions;

import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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
        try {
            if (project == null) {
                action.setChecked(false);
                return;
            }

            IProjectDescription ipd = project.getDescription();
            if (ipd.hasNature(PDT.NATURE_ID)) {
                removeJLMPNature(project);
                action.setChecked(false);
            } else {
                removeNatureFromAllOtherProjects();
                addJLMPNature(project);
                action.setChecked(true);
            }
            action
                    .setChecked(project.getDescription().hasNature(
                            PDT.NATURE_ID));
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

    /**
     * @param ipd
     * @return
     * @throws CoreException
     */
private void addJLMPNature(IProject project) throws CoreException {
        
        IProjectDescription ipd = project.getDescription();
        String[] oldNIDs = ipd.getNatureIds();
        String[] newNIDs = new String[oldNIDs.length + 1];
        newNIDs[0] = PDT.NATURE_ID;
        System.arraycopy(oldNIDs, 0, newNIDs, 1, oldNIDs.length);
		ipd.setNatureIds(newNIDs);
		project.setDescription(ipd, null);
        
    }
    /**
     * @param project
     * @return
     * @throws CoreException
     */
    private void removeJLMPNature(IProject project) throws CoreException {
        if (project.hasNature(PDT.NATURE_ID)) {
            IProjectDescription ipd = project.getDescription();
            String[] oldNIDs = ipd.getNatureIds();
            String[] newNIDs;
            newNIDs = new String[oldNIDs.length - 1];
            int j = 0;
            for (int i = 0; i < newNIDs.length; i++) {
                if (oldNIDs[j].equals(PDT.NATURE_ID))
                    j++;
                newNIDs[i] = oldNIDs[j];
                j++;
            }
            ipd.setNatureIds(newNIDs);
            project.setDescription(ipd, null);
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
                removeJLMPNature(projects[i]);
    }

    /*
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
                                PDT.NATURE_ID));
                    } catch (CoreException e) {
                        Debug.report(e);
                    }
                } else {
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