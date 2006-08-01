package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.ViewerFilter;

public abstract class PrologOutlineFilter extends ViewerFilter {
	private String id;

	private String label;

	public PrologOutlineFilter(String id, String label) {
		this.id = id;
		this.label = label;
	}

	public String getId() {
		return id;
	}

	public String getLabel() {

		return label;
	}

	@Override
	public String toString() {
		return getId();
	}
}
