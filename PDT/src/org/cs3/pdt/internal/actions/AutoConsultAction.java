/*
 */
package org.cs3.pdt.internal.actions;

import org.cs3.pdt.IPrologProject;
import org.cs3.pdt.PDT;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

/**
 */
public class AutoConsultAction implements IObjectActionDelegate {
    private IWorkbenchPart targetPart;

    private IAction action;

    private IFile file;

    private IPrologProject project;

    protected boolean error;

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
            if (file == null||project==null) {
                action.setChecked(false);
                action.setEnabled(false);
                return;
            }
            if(project.isAutoConsulted(file)){
                project.setAutoConsulted(file,false);
                action.setChecked(false);
            }
            else{
                project.setAutoConsulted(file,true);
                action.setChecked(true);
            }
            
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }
    }

    /*
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
     *           org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        try {
            if (selection instanceof IStructuredSelection) {
                Object obj = ((IStructuredSelection) selection)
                        .getFirstElement();
                if (obj instanceof IFile) {
                    file = (IFile) obj;

                    project = (IPrologProject) file.getProject()
                                                .getNature(PDT.NATURE_ID);
                    action.setEnabled(project != null
                            && project.isPrologSource(file));

                    action.setChecked(project!=null&&project.isAutoConsulted(file));
                }

                else {
                    file = null;
                    project=null;
                }
            } else {
                file = null;
                project=null;
            }
        } catch (Throwable t) {
            Debug.report(t);
            throw new RuntimeException(t);
        }

    }

}
