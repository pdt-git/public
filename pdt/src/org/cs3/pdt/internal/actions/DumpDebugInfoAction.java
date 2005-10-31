/*
 */
package org.cs3.pdt.internal.actions;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
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
		
		
		PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		PrologInterface pif = console.getPrologInterface();
		PrologSession session = pif.getSession();
		 try {
             
             List l = session.queryAll("current_thread(A,B)");
             for (Iterator iter = l.iterator(); iter.hasNext();) {
				Map r = (Map) iter.next();
				Debug.info(r.get("A")+"-->"+r.get("B"));
			}
             
             
         } catch (Throwable e) {
             Debug.report(e);
         }
         finally{
        	 if(session!=null){
        		 session.dispose();
        	 }
         }
        

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        // TODO Auto-generated method stub

    }

}
