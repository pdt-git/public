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

final class LexicalPrologOutlineSorter extends ViewerSorter {
	public int category(Object element) {
		if (element instanceof DirectiveNode) {
			return 0;
		}
		if (element instanceof ClauseNode) {
			return 1;
		}
		if (element instanceof PredicateNode) {
			return 2;
		}
		if (element instanceof CTermNode) {
			return 3;
		}
		return 4;
	}

	public int compare(Viewer viewer, Object e1, Object e2) {
		if (e1 instanceof DirectiveNode && e2 instanceof DirectiveNode) {
			return ((DirectiveNode) e1).getProperty("label").compareTo(((DirectiveNode)e2).getProperty("label"));
		}
		if (e1 instanceof Clause && e2 instanceof Clause) {
			return ((Comparable) e1).compareTo(e2);
		}
		if (e1 instanceof CTermNode && e2 instanceof CTermNode) {
			CTerm t1 = ((CTermNode) e1).term;
			CTerm t2 = ((CTermNode) e2).term;
			CCompound pos1 = (CCompound) t1.getAnotation("position");
			CCompound pos2 = (CCompound) t2.getAnotation("position");
			int TOP = Integer.MAX_VALUE;
			int start1 = TOP;
			int start2 = TOP;
			int end1 = TOP;
			int end2 = TOP;
			if (pos1 != null) { // can be null, e.g. for implicit NILs
				start1 = ((CInteger) pos1.getArgument(0)).getIntValue();
				end1 = ((CInteger) pos1.getArgument(1)).getIntValue();
			}
			if (pos2 != null) { // can be null, e.g. for implicit NILs
				start2 = ((CInteger) pos2.getArgument(0)).getIntValue();
				end2 = ((CInteger) pos2.getArgument(1)).getIntValue();
			}
			int c = start1 - start2;
			if (c != 0) {
				return c;
			}
			c = end1 - end2;
			if (c != 0) {
				return c;
			}

		}
		if (e1 instanceof Predicate && e2 instanceof Predicate) {
			Predicate p1 = (Predicate) e1;
			Predicate p2 = (Predicate) e2;
			return p1.compareTo(p2);
		}
		return super.compare(viewer, e1, e2);
	}
}