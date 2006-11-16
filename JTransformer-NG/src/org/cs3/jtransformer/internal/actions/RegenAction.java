package org.cs3.jtransformer.internal.actions;


import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.jtransformer.regenerator.SourceCodeRegenerator;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
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
public class RegenAction implements IWorkbenchWindowActionDelegate {
	
	private class RegenExecution implements IRunnableWithProgress {
		


		/**
		 * Iterates through the dirty classes, and generate a new java sourcefile
		 * for each that has been changed. Mark them as processed if all files
		 * are successfully replaced.
		 */
		


		public void run(IProgressMonitor monitor)  {
			
			SourceCodeRegenerator scr;
			try {
				PrologInterface pif = JTUtils.getPrologInterface(project);
				if(pif == null) {
					UIUtils.displayErrorDialog(JTUtils.getShell(true), "JTransformer",
							"The enclosing project of the currently opened file has no JTransformer Nature");
					return;
				}
				scr = new SourceCodeRegenerator(pif);
				scr.generateDirtyClasses();
			} catch (IOException e) {
				e.printStackTrace();
				Debug.report(e);
			} catch (CoreException e) {
				e.printStackTrace();
				Debug.report(e);
			} catch (PrologInterfaceException e)
			{
				UIUtils.logAndDisplayError(
						JTransformerPlugin.getDefault().getErrorMessageProvider(),
						UIUtils.getDisplay().getActiveShell(),
						JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
						JTransformer.ERR_CONTEXT_SOURCE_REGENERATION,
						e
						);
			}
		}
		
	}
	
	private IWorkbenchWindow window;
	private IProject project;
	/**
	 * constructs a new RegenButton
	 */
	
	public RegenAction() {
	}

	/**
	 * kicks of the regeneration process.
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run(IAction action)  {
		
		try {
			try {
				
				project = ((IFileEditorInput)JTUtils.getActiveEditor().getEditorInput()).getFile().getProject();
			} catch(final Exception ex) {
				ex.printStackTrace();
				UIUtils.getDisplay().asyncExec(new Runnable() {

					public void run() {
						UIUtils.displayErrorDialog(JTUtils.getShell(true), "JTransformer", 
								"This operation cannot proceed.\nNo source file associated with workspace project is opened.\n"+
								ex.getLocalizedMessage());							
					}
				
				});
				return;
			}

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
