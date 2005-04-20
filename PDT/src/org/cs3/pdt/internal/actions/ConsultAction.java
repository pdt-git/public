/*
 */
package org.cs3.pdt.internal.actions;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 */
public class ConsultAction implements IWorkbenchWindowActionDelegate {

    /* (non-Javadoc)
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
     */
    public void dispose() {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
     */
    public void init(IWorkbenchWindow window) {
        // TODO Auto-generated method stub

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
       PDTPlugin plugin = PDTPlugin.getDefault();
    IEditorInput input = UIUtils.getActiveEditor().getEditorInput();
       if (input==null){
           Debug.warning("Consult action triggered, but active editor input is null.");
       }
       if (input instanceof IFileEditorInput){
           IFileEditorInput fileInput = (IFileEditorInput) input;
           try {
            File file = fileInput.getFile().getLocation().toFile().getCanonicalFile();
            PrologSession session = plugin.getPrologInterface().getSession();
            session.queryOnce("['"+Util.prologFileName(file)+"']");
        } catch (IOException e) {
           Debug.report(e);
           throw new RuntimeException(e);
        }
           
       }
       else{
           Debug.warning("Consult action triggered, but active editor input is no file.");
       }
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        Debug.debug("selection changed");
    }

}
