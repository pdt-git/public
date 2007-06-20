package org.cs3.pdt.internal.views;

import org.eclipse.jface.viewers.Viewer;

public class HideDirectivesFilter extends PrologOutlineFilter {

	
	public HideDirectivesFilter(String id, String label) {
		super(id, label);
	}

	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if(element instanceof PEFNode){
			PEFNode p = (PEFNode) element;
			if("pef_toplevel".equals(p.getType()) && p.getTags().contains("directive")){			
				return false;
			}
		}
		return true;
	}

	

}
