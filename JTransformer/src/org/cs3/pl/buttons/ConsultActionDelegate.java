package org.cs3.pl.buttons;

import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologHelper;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class ConsultActionDelegate implements IWorkbenchWindowActionDelegate {
	private IWorkbenchWindow window;

	/**
	 *
	 */
	public ConsultActionDelegate() {
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		
		//The Thread is needed, because the consult processing may be part of  
		// a tracing/debugging session. The output the console must 
		// wait for the calling Runnable to finish -> deadlock in the Eclipse Display Scheduler
		Thread consulter =  new Thread(){
			public void run() {
				final IEditorPart editorPart = PDTPlugin.getDefault().getActiveEditor();
				boolean dirty = editorPart.isDirty();
				if (dirty) {
					PDTPlugin.getDefault().getDisplay().syncExec(new Runnable() {
						/* (non-Javadoc)
						 * @see java.lang.Runnable#run()
						 */
						public void run() {
							editorPart.doSave(null);
						}
					});
					
				}

				IPrologClient client;
				
					client = PDTPlugin.getDefault().getPrologClient();
					final String filename = PDTPlugin.getDefault().getActiveRawFileName();
/*					CTChecker checker = new CTChecker();
					checker.check(PDTPlugin.getDefault().getActiveFile());
*/					new PrologHelper(client).consult(filename);
					//if (dirty || manager.getCachedPrologElements(filename) == null && ret) {
					
			}
		};
		consulter.start();
		
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
		this.window = window;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
	 */
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// TODO Auto-generated method stub
		
	}
}
