package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;

public class SearchModuleElement implements PDTTreeElement {
	
	private List<PDTMatch> elements = new ArrayList<PDTMatch>();
	List<SearchPredicateElement> predicates = new ArrayList<SearchPredicateElement>();
	private String label;
	
	public SearchModuleElement(String name, String visibility) {
		if (visibility == null || visibility.isEmpty()) {
			label = name;
		} else {
			label = name + " (" + visibility + ")";
		}
	}
	
	public String getLabel() {
		return label;
	}
	
	public void addElement(PDTMatch elem) {
		elements.add(elem);
		
		SearchPredicateElement predicate = (SearchPredicateElement)elem.getElement();
		if (!predicates.contains(predicate)) {
			predicates.add(predicate);
		}
	}

	@Override
	public Object[] getChildren() {
		return predicates.toArray();
	}

	@Override
	public boolean hasChildren() {
		return !predicates.isEmpty();
	}

}

