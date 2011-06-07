package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;

public class SearchResultCategory implements PDTTreeElement {
	private String categoryName;
	private List<PrologMatch> matches = new ArrayList<PrologMatch>();
	Map<String, ModuleSearchElement> elements= new HashMap<String, ModuleSearchElement>();
	
	public SearchResultCategory(String name) {
		categoryName = name;
	}
	
	public String getLabel() {
		return categoryName;
	}

	public void addMatch(PrologMatch match) {
		matches.add(match);
		addElement(match);
	}
	
	public List<PrologMatch> getMatches() {
		return matches;
	}
	
	public void addElement(PrologMatch match) {
		String moduleName = match.getModule();
			
		if (!elements.containsKey(moduleName)) {
			ModuleSearchElement newModule = new ModuleSearchElement(moduleName);
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
