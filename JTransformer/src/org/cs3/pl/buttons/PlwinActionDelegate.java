/*
 * Created on 05.10.2004
 *
 */
package org.cs3.pl.buttons;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @author rho
 *
 */
public class PlwinActionDelegate implements IWorkbenchWindowActionDelegate {

    /* 
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
     */
    public void dispose() {
    }

    /* 
     * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
     */
    public void init(IWorkbenchWindow window) {
    }

    /* 
     * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
     */
    public void run(IAction action) {
        final String tmpDir = PDTPlugin.getDefault().getStateLocation().toFile().getAbsolutePath()+ File.separator;
        final String currentTreeFacts = tmpDir + "currentTreeFacts.pl";
       
        try {
            PrologManager.getInstance().getHiddenClient().query("writeTreeFacts('" + PrologHelper.makeFilenameSWIConform(currentTreeFacts) + "')");
	        String goal = "consult('" + PrologManager.getEngineDir() + "main.pl'),"+
	                      "consult('"+ PrologHelper.makeFilenameSWIConform(currentTreeFacts)+"').";
			String exec = "plwin -g " + goal;
			System.out.println(exec);
			Runtime thisRuntime = Runtime.getRuntime();
			String prologBin;
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


    /* 
     * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
     */
    public void selectionChanged(IAction action, ISelection selection) {
    }

}
