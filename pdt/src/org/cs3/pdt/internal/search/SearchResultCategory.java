package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.List;

public class SearchResultCategory {
	private String categoryName;
	private List<PrologMatch> matches = new ArrayList<PrologMatch>();
	
	public SearchResultCategory(String name) {
		categoryName = name;
	}
	
	public String getLabel() {
		return categoryName;
	}

	public void addMatch(PrologMatch match) {
		matches.add(match);
	}
	
	public List<PrologMatch> getMatches() {
		return matches;
	}
	
}
