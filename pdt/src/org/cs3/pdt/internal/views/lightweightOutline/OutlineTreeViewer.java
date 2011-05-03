package org.cs3.pdt.internal.views.lightweightOutline;

import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.widgets.Tree;

class OutlineTreeViewer extends TreeViewer {

	private boolean fIsFiltering= false;

	OutlineTreeViewer(Tree tree) {
		super(tree);

	}

	/**
	 * {@inheritDoc}
	 */
	protected Object[] getFilteredChildren(Object parent) {
		Object[] result = getRawChildren(parent);
		int unfilteredChildren= result.length;
		ViewerFilter[] filters = getFilters();
		if (filters != null) {
			for (int i= 0; i < filters.length; i++)
				result = filters[i].filter(this, parent, result);
		}
		fIsFiltering= unfilteredChildren != result.length;
		return result;
	}



	
}