package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class SearchResultCategory implements PDTTreeElement {
	private String categoryName;
	private List<PDTMatch> matches = new ArrayList<PDTMatch>();
	Map<String, SearchModuleElement> elements= new HashMap<String, SearchModuleElement>();
	
	public SearchResultCategory(String name) {
		categoryName = name;
	}
	
	public String getLabel() {
		return categoryName;
	}

	public void addMatch(PDTMatch match) {
		matches.add(match);
		addElement(match);
	}
	
	public List<PDTMatch> getMatches() {
		return matches;
	}
	
	public void addElement(PDTMatch match) {
		String moduleName = match.getModule();
			
		if (!elements.containsKey(moduleName)) {
			SearchModuleElement newModule = new SearchModuleElement(moduleName);
			elements.put(moduleName, newModule);
		}
		elements.get(moduleName).addElement(match);
	}

	@Override
	public boolean hasChildren() {
		return !elements.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return elements.values().toArray();
	}
	
}
