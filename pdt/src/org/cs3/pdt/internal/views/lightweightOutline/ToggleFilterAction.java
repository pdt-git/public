package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerFilter;

public class ToggleFilterAction extends Action {
	
	private TreeViewer viewer;
	private ViewerFilter filter;

	ToggleFilterAction(String text, TreeViewer viewer, ViewerFilter filter) {
		super(text, AS_CHECK_BOX);
		setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.FILTER));
		setChecked(false);
		this.viewer = viewer;
		this.filter = filter;
	}
	
	@Override
	public void run() {
		if (isChecked()) {
			viewer.addFilter(filter);
		} else {
			viewer.removeFilter(filter);
		}
	}

}
