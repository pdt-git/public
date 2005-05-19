package org.cs3.pdt.internal.views;

import org.eclipse.jface.resource.ImageDescriptor;

public class QueryConsoleThreadAction extends QueryAction {

	public QueryConsoleThreadAction(String query, String text, String tooltip,
			ImageDescriptor icon) {
		super(query, text, tooltip, icon);
	}

	protected String buildQuery() {
		return "current_thread(ThreadId,B), "
				+ "atom_concat('client@',_,ThreadId),"
				+ "thread_signal(ThreadId,(" + getQuery() + "))";
	}

}