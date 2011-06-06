/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.cs3.pdt.internal.views.lightweightOutline.OutlinePredicate;
import org.cs3.pdt.internal.views.lightweightOutline.PredicateOccuranceElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class PositionalPrologOutlineSorter extends ViewerSorter {
	@Override
	public int category(Object element) {
		
		if(element instanceof OutlinePredicate) {
			return 2;
		}
		
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
		
		if ((e1 instanceof OutlinePredicate) && (e2 instanceof OutlinePredicate)) {
			return ((OutlinePredicate)e1).getLine() - ((OutlinePredicate)e2).getLine();
		}
		if ((e1 instanceof PredicateOccuranceElement) && (e2 instanceof PredicateOccuranceElement)) {
			PredicateOccuranceElement occ1 = (PredicateOccuranceElement)e1;
			PredicateOccuranceElement occ2 = (PredicateOccuranceElement)e2;
			if (occ1.getType().equals(occ2.getType()))
				return (occ1).getLine() - (occ2).getLine();
			if ((occ1.getType().equals("declaration")) || (occ2.getType().equals("multifile")))
				return -1;
			if ((occ1.getType().equals("multifile")) || (occ1.getType().equals("declaration")))
				return 1;
		}
		if (e1 instanceof PEFNode && e2 instanceof PEFNode) {
			return ((PEFNode)e1).getStartPosition()-((PEFNode)e2).getStartPosition();
		}				
		return super.compare(viewer, e1, e2);
	}
}