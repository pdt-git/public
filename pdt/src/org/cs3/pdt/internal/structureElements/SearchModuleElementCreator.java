package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class SearchModuleElementCreator {
	
	public static SearchModuleElement[] getModuleDummiesForMatches(List<PDTMatch> matches) {
		if (matches == null) {
			return new SearchModuleElement[0];
		}
		Map<String, SearchModuleElement> dummies= new HashMap<String, SearchModuleElement>();
		for (PDTMatch match:matches) {
			String moduleName = match.getModule();
			
			if (!dummies.containsKey(moduleName)) {
				SearchModuleElement newModule = new SearchModuleElement(moduleName);
				dummies.put(moduleName, newModule);
			}
			dummies.get(moduleName).addElement(match);
		}
		
		List<SearchModuleElement> moduleList = new ArrayList<SearchModuleElement>();
		Set<String> keys = dummies.keySet();
		for(String key: keys) {
			moduleList.add(dummies.get(key));
		}
		
		return ((SearchModuleElement[])moduleList.toArray(new SearchModuleElement[0]));
	}

}
