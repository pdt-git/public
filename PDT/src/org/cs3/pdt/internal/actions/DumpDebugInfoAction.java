/*
 */
package org.cs3.pdt.internal.actions;

import java.io.IOException;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 */
public class DumpDebugInfoAction implements IWorkbenchWindowActionDelegate {

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
		PrologInterface pif = null;
		try {
			pif = plugin.getPrologInterface();
		} catch (IOException e) {
			Debug.report(e);
		}
		 try {
             PrologSession session = pif.getSession();
             Map r = session.query("current_thread(A,B)");
             while(r!=null){
                 Debug.info(r.get("A")+"-->"+r.get("B"));
                 r=session.next();
             }
             session.dispose();
         } catch (Throwable e) {
             Debug.report(e);
         }
        

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        // TODO Auto-generated method stub

    }

}
