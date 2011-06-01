package org.cs3.pdt.internal.views.lightweightOutline;

import org.eclipse.jface.viewers.ILabelProvider;
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
	public boolean select(Viewer viewer, Object parentElement, Object element) {
		StringMatcher matcher= this.abstractInformationControl.getMatcher();
		if (matcher == null || !(viewer instanceof TreeViewer))
			return true;
		TreeViewer treeViewer= (TreeViewer) viewer;

		String matchName= ((ILabelProvider) treeViewer.getLabelProvider()).getText(element);
		matchName= TextProcessor.deprocess(matchName);
		if (matchName != null && matcher.match(matchName))
			return true;

		return false;
	}


}