/*
 * Created on 03.09.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.buttons;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @author rho
 *
 */
public class HelpActionDelegate implements IWorkbenchWindowActionDelegate {

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
		String exec = "plwin -g help";
		Runtime thisRuntime = Runtime.getRuntime();
		String prologBin;
        try {
            prologBin = PrologManager.getProjectDir() + "swipl" + File.separator + "bin";
            Process helpProcess = thisRuntime.exec(
                    prologBin+File.separator +exec, null,
    				new File(prologBin));
        } catch (FileNotFoundException e) {
            Debug.report(e);
        } catch (IOException e) {
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
