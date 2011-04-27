package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class CategoryHandler {
	private Map<String, SearchResultCategory> categories = new HashMap<String,SearchResultCategory>();
	
	
	public SearchResultCategory[] getCategories(){
		List<SearchResultCategory> allCategories = new ArrayList<SearchResultCategory>();
		
		Set<String> keys = categories.keySet();
		for(String key:keys) {
			allCategories.add(categories.get(key));
		}
		return (SearchResultCategory[])allCategories.toArray();
	}
	
	public void addMatchToCategory(PrologMatch match, String categoryName) {
		if (categories.containsKey(categoryName)) {
			categories.get(categoryName).addMatch(match);
		} else {
			SearchResultCategory newCategory = new SearchResultCategory(categoryName);
			newCategory.addMatch(match);
			categories.put(categoryName, newCategory);
		}
	}
}
