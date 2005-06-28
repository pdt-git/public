package org.cs3.jlmp.internal.actions;


import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.internal.natures.JLMPProjectNature;
import org.cs3.jlmp.regenerator.SourceCodeRegenerator;
import org.cs3.pl.common.Debug;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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
public class RegenAction implements IWorkbenchWindowActionDelegate {
	
	private class RegenExecution implements IRunnableWithProgress {
		

		/**
		 * Iterates through the dirty classes, and generate a new java sourcefile
		 * for each that has been changed. Mark them as processed if all files
		 * are successfully replaced.
		 */
		
		public JLMPProjectNature getProjectWithJLMPNature() throws CoreException {
			JLMPProjectNature nature = null;
			IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
			for (int i = 0; i < projects.length; i++) {
				if(projects[i].isAccessible() && projects[i].hasNature(JLMP.NATURE_ID)){
					nature = (JLMPProjectNature)projects[i].getNature(JLMP.NATURE_ID);
					break;
				}
			}
			return nature;
		}

		public void run(IProgressMonitor monitor)  {
			
			SourceCodeRegenerator scr;
			try {
				scr = new SourceCodeRegenerator(getProjectWithJLMPNature().getPrologInterface());
				scr.generateDirtyClasses();
			} catch (IOException e) {
				e.printStackTrace();
				Debug.report(e);
			} catch (CoreException e) {
				e.printStackTrace();
				Debug.report(e);
			}
		}
		
	}
	
	private IWorkbenchWindow window;
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
