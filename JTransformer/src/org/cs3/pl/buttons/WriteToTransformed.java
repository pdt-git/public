package org.cs3.pl.buttons;

import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class WriteToTransformed implements IWorkbenchWindowActionDelegate {
	/**
	 *
	 */
	public WriteToTransformed() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(final IAction action){
		PDTPlugin.getDefault().getDisplay().asyncExec(new Runnable(){
			
			public void run() {
				try {
					_run();
				} catch (IOException e) {
					Debug.report(e);
				} catch (CoreException e) {
					// TODO Auto-generated catch block
					Debug.report(e);
				}
			}});
	}

	public void _run() throws IOException, CoreException {		
		IPrologClient pm = PrologManager.getInstance().getHiddenClient();
		PrologManager.getInstance().showOnConsole();
		// FIX JT-65: remove dot at the end of the query.
		pm.query("gen_toplevels");
		PrologManager.getInstance().doNotShowOnConsole();

		PDTPlugin.getDefault().getWorkspace().getRoot().getProject(PDTPlugin.TRANSFORMED).refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection)  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose()  {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#init
	 */
	public void init(IWorkbenchWindow window)  {
	}
}
