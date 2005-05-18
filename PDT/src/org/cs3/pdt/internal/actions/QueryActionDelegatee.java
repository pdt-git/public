package org.cs3.pdt.internal.actions;


import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.search.PrologSearchQuery;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @author rho
 *
 */
public class QueryActionDelegatee implements IWorkbenchWindowActionDelegate {

	private String query;
	private String progressInfo;
	
	public QueryActionDelegatee(String query,String progressInfo) {
		this.query = query;
		this.progressInfo = progressInfo;
	}
    public void dispose() {

    }

    
    public void init(IWorkbenchWindow window) {

    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
        try {

            Job j = new Job(progressInfo) {

                protected IStatus run(IProgressMonitor monitor) {
                    try {

                        PrologInterface prologInterface = PDTPlugin
                                .getDefault().getPrologInterface();
                        PrologSession session = prologInterface.getSession();
                        try{
                            session.queryOnce("thread_signal(main,("+query+"))");
                        }
                        finally{
                            session.dispose();
                        }
                    } catch (Throwable e) {
                        Debug.report(e);
                        return Status.CANCEL_STATUS;
                    } finally {
                        monitor.done();
                    }
                    return Status.OK_STATUS;
                }
            };
            j.schedule();
        } catch (Throwable t) {
            Debug.report(t);
        }
    }

    /* (non-Javadoc)
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
        // TODO Auto-generated method stub

    }

}