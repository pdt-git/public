package org.cs3.pl.buttons;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.cs3.pl.Debug;
import org.cs3.pl.exceptions.ExceptionHandler;
import org.cs3.pl.regenerator.SourceCodeRegenerator;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;

/**
 * This class implements the regeneration of java sourcecode after a CT has been
 * applied in the Prolog world. The class is replaced by using a 
 * SourceCodeRegenerator. On click, the Prolog System is queried for dirty
 * trees, and the corresponding classes are collected. For each of them, the
 * new sourcecode is generated.
 * 
 * @see IWorkbenchWindowActionDelegate
 */
public class RegenButton implements IWorkbenchWindowActionDelegate {
	
	private class RegenExecution implements IRunnableWithProgress {
		

		/**
		 * Iterates through the dirty classes, and generate a new java sourcefile
		 * for each that has been changed. Mark them as processed if all files
		 * are successfully replaced.
		 */
		
		public void run(IProgressMonitor monitor)  {
			
			SourceCodeRegenerator scr;
			try {
				scr = new SourceCodeRegenerator();
				scr.generateDirtyClasses();
			} catch (IOException e) {
				ExceptionHandler.handle(e);
			}
		}
		
	}
	
	private IWorkbenchWindow window;
	/**
	 * constructs a new RegenButton
	 */
	
	public RegenButton() {
	}

	/**
	 * kicks of the regeneration process.
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		
		try {
			window.run(true,true, new RegenExecution());
		} catch (InvocationTargetException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			Debug.report(e);
		}
		
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
	 * initializes the Workbenchwindow we are working in.
	 */
	public void init(IWorkbenchWindow window)  {
		this.window = window;
	}
}
