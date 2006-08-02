/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class PositionalPrologOutlineSorter extends ViewerSorter {
	public int category(Object element) {
		if (element instanceof DirectiveNode|| element instanceof PredicateNode) {
			return 0;
		}
		if (element instanceof ClauseNode) {
			return 1;
		}
		
		if (element instanceof CTermNode) {
			return 2;
		}
		return 3;
	}

	public int compare(Viewer viewer, Object e1, Object e2) {
		if (e1 instanceof Positional && e2 instanceof Positional) {
			return ((Positional)e1).getPositions()[0]-((Positional)e2).getPositions()[0];
		}				
		return super.compare(viewer, e1, e2);
	}
}