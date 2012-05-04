package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;

public class SearchModuleElement implements PDTTreeElement, Comparable<SearchModuleElement> {
	
	private List<PDTMatch> elements = new ArrayList<PDTMatch>();
	List<SearchPredicateElement> predicates = new ArrayList<SearchPredicateElement>();
	private String label;
	private int visibilityCode;
	
	public SearchModuleElement(String name, String visibility) {
		if (visibility == null || visibility.isEmpty()) {
			label = name;
		} else {
			label = name + " (" + visibility + ")";
		}
		if ("invisible".equalsIgnoreCase(visibility)) {
			visibilityCode = 1; 
		} else if ("sub".equalsIgnoreCase(visibility)) {
			visibilityCode = 2; 
		} else if ("local".equalsIgnoreCase(visibility)) {
			visibilityCode = 3; 
		} else if ("super".equalsIgnoreCase(visibility)) {
			visibilityCode = 4; 
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

	@Override
	public int compareTo(SearchModuleElement o) {
		return o.visibilityCode - this.visibilityCode;
	}

}

