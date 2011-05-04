package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.ArrayList;
import java.util.List;


public class PrologSourceFileModel {
	
	private List<OutlinePredicate> predicates= new ArrayList<OutlinePredicate>();

	public PrologSourceFileModel(List<OutlinePredicate> predicates) {
		super();
		this.predicates = predicates;
	}

	public List<OutlinePredicate> getPredicates() {
		return predicates;
	}
	
	public void dispose() {
		predicates.clear();
	}
}
