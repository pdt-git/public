/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.cs3.pdt.internal.views.lightweightOutline.PredicateOccuranceElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class LexicalPrologOutlineSorter extends ViewerSorter {
	@Override
	public int category(Object element) {

		if (!(element instanceof PEFNode)) {
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

			PEFNode node1 = ((PEFNode) e1);
			PEFNode node2 = ((PEFNode) e2);
			if ("pef_predicate".equals(node1.getType())
					&& "pef_predicate".equals(node2.getType())) {
				return node1.getLabel().compareTo(node2.getLabel());
			} else if ("pef_toplevel".equals(node1.getType())
					&& "pef_toplevel".equals(node2.getType())) {
				return node1.getStartPosition() - node2.getStartPosition();
			}
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
		return super.compare(viewer, e1, e2);
	}
}