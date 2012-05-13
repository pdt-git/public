package org.cs3.pdt.internal.structureElements;

import java.util.LinkedHashMap;

public class SearchModuleElement implements PDTSearchTreeElement, Comparable<SearchModuleElement> {
	
	private String label;
	private int visibilityCode;
	private String name;
	private Object parent;
	
	private LinkedHashMap<String, SearchPredicateElement> predForSignature = new LinkedHashMap<String, SearchPredicateElement>();
	
	public SearchModuleElement(Object parent, String name, String visibility) {
		this.parent = parent;
		this.name = name;
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
	
	public String getName() {
		return name;
	}
	
	@Override
	public Object[] getChildren() {
		return predForSignature.values().toArray();
	}

	@Override
	public boolean hasChildren() {
		return !predForSignature.isEmpty();
	}

	@Override
	public int compareTo(SearchModuleElement o) {
		return o.visibilityCode - this.visibilityCode;
	}

	@Override
	public void removeMatch(PDTMatch match) {
		String signature = getSignatureForMatch(match);
		predForSignature.get(signature);
		if (predForSignature.containsKey(signature)) {
			SearchPredicateElement predicateElement = predForSignature.get(signature);
			predicateElement.removeMatch(match);
			if (!predicateElement.hasChildren()) {
				predForSignature.remove(signature);
			}
		}
	}

	@Override
	public void addMatch(PDTMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement searchPredicateElement = predForSignature.get(signature); 
		if (searchPredicateElement == null) {
			searchPredicateElement = new SearchPredicateElement(this, match.getModule(), match.getName(), match.getArity(), match.getProperties());
			predForSignature.put(signature, searchPredicateElement);
		}
		searchPredicateElement.addMatch(match);
	}
	
	private String getSignatureForMatch(PDTMatch match) {
		return match.getDeclOrDef() + match.getName() + match.getArity();
	}

	@Override
	public Object getParent() {
		return parent;
	}

}

