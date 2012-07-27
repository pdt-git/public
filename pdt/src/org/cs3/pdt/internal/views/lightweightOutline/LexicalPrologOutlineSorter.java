/**
 * 
 */
package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.PredicateOccuranceElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class LexicalPrologOutlineSorter extends ViewerSorter {

	@Override
	public int compare(Viewer viewer, Object e1, Object e2) {
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