/*
 * Created on 09.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.views;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.internal.ViewPluginAction;
import org.eclipse.ui.texteditor.MarkerUtilities;

/**
 * @author rho
 *
 */
public class AutomaticConsultActionDelegatee implements IViewActionDelegate {

    /* (non-Javadoc)
     * @see org.eclipse.ui.IViewActionDelegate#init(org.eclipse.ui.IViewPart)
     */
    public void init(IViewPart view) {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
       System.out.println("run");
       //ViewPluginAction vpAction = (ViewPluginAction)action;
       		IFile file = (IFile)((IStructuredSelection)selection).getFirstElement();
       		
       		if(file.getMarker(CONSULTMARKER)==null)
                try {
                    MarkerUtilities.createMarker(file,null,"type");
                } catch (CoreException e) {
                    e.printStackTrace();
                }
       }
    
    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    private ISelection selection;
    private static final long CONSULTMARKER = 1112121212;
    
    public void selectionChanged(IAction action, ISelection selection) {
        this.selection = selection;
        ViewPluginAction vpAction = (ViewPluginAction)action;
   		IFile file = (IFile)((IStructuredSelection)selection).getFirstElement();
        if(file.getMarker(CONSULTMARKER)!= null)
            vpAction.setChecked(true);
        else
            vpAction.setChecked(false);
    }

}
