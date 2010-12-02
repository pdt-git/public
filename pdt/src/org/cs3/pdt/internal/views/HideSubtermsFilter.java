package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.Viewer;

public class HideSubtermsFilter extends PrologOutlineFilter {

	public HideSubtermsFilter(String id, String label) {
		super(id, label);
	}

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		return true;
	}

}
