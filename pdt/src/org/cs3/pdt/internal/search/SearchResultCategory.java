package org.cs3.pdt.internal.search;

public class SearchResultCategory {
	private String categoryName;
	
	public SearchResultCategory(String name) {
		categoryName = name;
	}
	
	public String getLabel() {
		return categoryName;
	}

}
