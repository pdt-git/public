package org.cs3.pdt.internal;

import org.eclipse.jface.resource.ImageDescriptor;

public class QueryMainThreadAction extends QueryAction {

	public QueryMainThreadAction(String query, String text, String tooltip, ImageDescriptor icon) {
		super(query, text, tooltip, icon);
	}

	 protected String buildQuery() {
		return "thread_signal(main,(" + getQuery()+ "))";

	}
}
