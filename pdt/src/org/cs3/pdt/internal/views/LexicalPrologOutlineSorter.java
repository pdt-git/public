/**
 * 
 */
package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;

final class LexicalPrologOutlineSorter extends ViewerSorter {
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
		return super.compare(viewer, e1, e2);
	}
}