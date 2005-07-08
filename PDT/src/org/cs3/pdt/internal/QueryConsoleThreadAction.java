package org.cs3.pdt.internal;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PrologConsole;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.ConsoleModel;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public class QueryConsoleThreadAction extends Action{

	private String query;
	private final PrologConsole console;
	

	public QueryConsoleThreadAction(PrologConsole console, String query,
			String text, String tooltip, ImageDescriptor icon) {
		super(text,icon);
		setToolTipText(tooltip);
		this.console=console;
		setQuery(query);		
		
	}

	public QueryConsoleThreadAction(String query,
			String text, String tooltip, ImageDescriptor icon) {		
		super(text,icon);
		setToolTipText(tooltip);
		setQuery(query);		
		this.console=null;
	}


	public void run() {
		try {

			Job j = new Job(getToolTipText()) {

				protected IStatus run(IProgressMonitor monitor) {
					try {
						PrologConsole c = getConsole();
						ConsoleModel model = c.getModel();
						model.setLineBuffer(" ");
						model.commitLineBuffer();
						model.setLineBuffer(getQuery());
						model.commitLineBuffer();
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



	public String getQuery() {
		return query;
	}

	private PrologConsole getConsole() {
		PrologConsole c = console;
		PDTPlugin plugin = PDTPlugin.getDefault();
		if(c==null){
			
			c=plugin.getPrologConsoleService().getActivePrologConsole();
		}
		if(c==null){
			try {
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().
				getActivePage().showView(PDT.CONSOLE_VIEW_ID);
				c=plugin.getPrologConsoleService().getActivePrologConsole();
			} catch (PartInitException e) {
				e.printStackTrace();
			}
		}
		return c;
	}


	public void setQuery(String query) {
		if(query==null){
			return;
		}
		if(!query.trim().endsWith(".")){
			this.query=query+".";
		}
		else{
			this.query = query;	
		}
	}

	


}