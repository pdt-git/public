/*
 * Created on 15.08.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.builders;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.prolog.IPrologClient;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;

/**
 * @author xproot
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class UnresolvedTypeLoader extends FactBaseBuilder {
	
	private Object factGenerationMonitor = new Object();
	private boolean factGenerationFinished = false;
    private static IProject transproject;

	/**
	 * @param project
	 */
	public UnresolvedTypeLoader(IPrologClient client) {
		super(transproject = ResourcesPlugin.getWorkspace().getRoot().getProject("Transformed"),client);
	}

	public void initFactbase() throws IOException, CoreException,
			InterruptedException {
		//		WorkbenchJob job = new WorkbenchJob("Initialize Factbase ...") {
		//
		//			public IStatus runInUIThread(IProgressMonitor monitor) {
		//				try {
		final IRunnableWithProgress op = new IRunnableWithProgress() {

			public void run(IProgressMonitor monitor)
					throws InvocationTargetException, InterruptedException {
				try {
				    
					loadExternalFacts(monitor);
					synchronized(factGenerationMonitor) {
						factGenerationFinished = true;
						factGenerationMonitor.notifyAll();
					}
				} catch (IOException e) {
					Debug.error(e.getLocalizedMessage());
				} catch (CoreException e) {
					Debug.error(e.getLocalizedMessage());
				}
			}
		};
		PDTPlugin.getDefault().getDisplay().syncExec(new Runnable() {

			public void run() {
		 	     try {
		 	     	new ProgressMonitorDialog(PDTPlugin.getShell()).run(false, true, op);					
				} catch (InvocationTargetException e) {
		 	       Debug.report(e);
		 	    } catch (InterruptedException e) {
			 	       Debug.report(e);
		 	    }
			}
		});
		synchronized(factGenerationMonitor){
			if (!factGenerationFinished) {
				Debug.error("error fact generation, not synchronized");
				factGenerationMonitor.wait();
			}
		}
	}
}