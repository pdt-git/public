package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.Viewer;

public class HidePrivatePredicatesFilter extends PrologOutlineFilter {

	public HidePrivatePredicatesFilter(String id, String label) {
		super(id, label);
	}

	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if(element instanceof PEFNode){
			PEFNode p = (PEFNode) element;
			if("pef_predicate".equals(p.getType()) && !p.getTags().contains("public")){			
				return false;
			}
		}
		return true;
	}

}
