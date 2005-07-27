/*
 */
package org.cs3.pdt.internal.actions;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 */
public class RestartAction implements IWorkbenchWindowActionDelegate {

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
     */
    public void dispose() {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
     */
    public void init(IWorkbenchWindow window) {
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
        runJob();
    }

	/**
	 * 
	 */
	public void runJob() {
		try {

            Job j = new Job("Restarting the PrologInterface") {

                protected IStatus run(IProgressMonitor monitor) {
                    try {
                        monitor.beginTask("initializing...",
                                IProgressMonitor.UNKNOWN);
						PDTPlugin r = PDTPlugin
                                .getDefault();

                        PrologInterface prologInterface = PrologRuntimePlugin.getDefault().getPrologInterface();
                        try{
                            prologInterface.stop();
                        }
                        finally{
                            prologInterface.start();
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

    /*
     * (non-Javadoc)
     * 
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
     *         org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
    }

}
