package org.cs3.pl.buttons;

import org.cs3.pl.PDTPlugin;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class DebugButton implements IWorkbenchWindowActionDelegate {
	
	static public boolean debugmode = false;
	
	/**
	 *
	 */
	public DebugButton() {
	}
	
	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		Thread thread =  new Thread(){
			public void run() {
				
				if (debugmode) {
					PDTPlugin.getDefault().getPrologClient().query("nodebug");
					debugmode = false;
					appendToConsole("nodebug");
				}
				else {
					PDTPlugin.getDefault().getPrologClient().query("debug");
					appendToConsole("debug");
					debugmode = true;
				}
			}
		};
		thread.start();
		
	}
	
	/**
	 * @param string
	 */
	protected void appendToConsole(String string) {
		PDTPlugin.getDefault().getPrologConsole().appendToPrologConsole(string +"\n");
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
