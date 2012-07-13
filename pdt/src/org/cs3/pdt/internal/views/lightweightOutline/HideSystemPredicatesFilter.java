package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.internal.structureElements.OutlinePredicateElement;
import org.cs3.pdt.internal.structureElements.OutlineClauseElement;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;

public class HideSystemPredicatesFilter extends ViewerFilter {

	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if (element instanceof OutlineClauseElement) {
			return true;
		} else if (element instanceof OutlinePredicateElement) {
			OutlinePredicateElement p = (OutlinePredicateElement) element;
			return !p.getFunctor().startsWith("$");
		} else if (element instanceof OutlineModuleElement) {
			OutlineModuleElement m = (OutlineModuleElement) element;
			for (Object child: m.getChildren()) {
				if (select(viewer, element, child)) {
					return true;
				}
			}
			return false;
		} else {
			return true;
		}
	}

}
