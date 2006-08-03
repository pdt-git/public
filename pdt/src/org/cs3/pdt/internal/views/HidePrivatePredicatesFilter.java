package org.cs3.pdt.internal.views;

import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.Viewer;

public class HidePrivatePredicatesFilter extends PrologOutlineFilter {

	public HidePrivatePredicatesFilter(String id, String label) {
		super(id, label);
	}

	public boolean select(Viewer viewer, Object parentElement, Object element) {
		if(element instanceof PredicateNode){
			PredicateNode p = (PredicateNode) element;
			if(!("user".equals(p.getModule()))&& ! "true".equals(p.getPredicateProperty(Predicate.EXPORTED))){
				return false;
			}
		}
		return true;
	}

}
