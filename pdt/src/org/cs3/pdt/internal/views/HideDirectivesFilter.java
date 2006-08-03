package org.cs3.pdt.internal.views;

import org.cs3.pl.metadata.Directive;
import org.eclipse.jface.viewers.Viewer;

public class HideDirectivesFilter extends PrologOutlineFilter {

	
	public HideDirectivesFilter(String id, String label) {
		super(id, label);
	}

	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if(element instanceof Directive){
			return false;
		}
		return true;
	}

	

}
