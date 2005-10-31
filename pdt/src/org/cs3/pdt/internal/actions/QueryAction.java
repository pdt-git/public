package org.cs3.pdt.internal.actions;


import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.resource.ImageDescriptor;

public class QueryAction extends Action {
	private String tooltip;

	String text;

	private String query;

	private ImageDescriptor icon;

	private final PrologInterface pif;

	public QueryAction(PrologInterface pif, String query, String text, String tooltip,
			ImageDescriptor icon) {
		this.pif = pif;
		this.query = query;
		this.text = text;
		this.tooltip = tooltip;
		this.icon = icon;
	}

	public ImageDescriptor getImageDescriptor() {
		return icon;
	}

	public String getToolTipText() {
		return tooltip;
	}

	public void run() {
		try {

			Job j = new Job(tooltip) {

				protected IStatus run(IProgressMonitor monitor) {
					try {

						PrologSession session = pif.getSession();
						try {
							//	 Alternative                        	
							//	                    		model.setLineBuffer(query + ".");
							//	                    		model.commitLineBuffer();
							session.queryOnce(buildQuery());

						} finally {
							session.dispose();
						}
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

	public String getText() {
		return text;
	}

	/**
	 * Template method to create a different query, 
	 * e.g. choose different thread to execute the query on.
	 * @return
	 */
	protected String buildQuery() {
		return query;
	}

	/**
	 * @return the pure query
	 */
	public String getQuery() {
		return query;
	}


	public void setQuery(String query) {
		this.query = query;
	}

	
}
