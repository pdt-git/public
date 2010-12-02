/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class PositionalPrologOutlineSorter extends ViewerSorter {
	@Override
	public int category(Object element) {
		
		
		if(! (element instanceof PEFNode  ) ){
			return 0;
		}
		PEFNode node = (PEFNode) element;
		if ("pef_toplevel".equals(node.getType())) {
			return 1;
		}
		
		if ("pef_predicate".equals(node.getType())) {
			return 2;
		}
		return 3;
	}

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
		
		if (e1 instanceof PEFNode && e2 instanceof PEFNode) {
			return ((PEFNode)e1).getStartPosition()-((PEFNode)e2).getStartPosition();
		}				
		return super.compare(viewer, e1, e2);
	}
}