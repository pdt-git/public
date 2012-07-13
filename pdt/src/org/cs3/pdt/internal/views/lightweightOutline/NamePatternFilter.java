package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.internal.structureElements.OutlinePredicateElement;
import org.cs3.pdt.internal.structureElements.OutlineClauseElement;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.osgi.util.TextProcessor;

/**
 * The NamePatternFilter selects the elements which
 * match the given string patterns.
 *
 * @since 2.0
 */
class NamePatternFilter extends ViewerFilter {

	private final AbstractInformationControl abstractInformationControl;
//	private final StringMatcher matcher;

	public NamePatternFilter(AbstractInformationControl abstractInformationControl, StringMatcher matcher) {
		this.abstractInformationControl = abstractInformationControl;
//		this.matcher = matcher;
	}

	/*
	 * @see org.eclipse.jface.viewers.ViewerFilter#select(org.eclipse.jface.viewers.Viewer, java.lang.Object, java.lang.Object)
	 */
	@Override
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		StringMatcher matcher= this.abstractInformationControl.getMatcher();
		if (matcher == null || !(viewer instanceof TreeViewer))
			return true;

		String matchName = null;
		if (element instanceof OutlineClauseElement) {
			return true;
		} else if (element instanceof OutlinePredicateElement) {
			OutlinePredicateElement p = (OutlinePredicateElement) element;
			matchName = p.getFunctor();
		} else if (element instanceof OutlineModuleElement) {
			OutlineModuleElement m = (OutlineModuleElement) element;
			matchName = m.getLabel();
		}
		matchName= TextProcessor.deprocess(matchName);
		if (matchName != null && matcher.match(matchName))
			return true;

		if (element instanceof OutlineModuleElement) {
			OutlineModuleElement e = (OutlineModuleElement) element;
			for (Object child: e.getChildren()) {
				if (select(viewer, element, child)) {
					return true;
				}
			}
		}
		return false;
	}

}