package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ModuleSearchElementCreator {
	
	public static ModuleSearchElement[] getModuleDummiesForMatches(List<PrologMatch> matches) {
		if (matches == null) {
			return new ModuleSearchElement[0];
		}
		Map<String, ModuleSearchElement> dummies= new HashMap<String, ModuleSearchElement>();
		for (PrologMatch match:matches) {
			String moduleName = match.getModule();
			
			if (!dummies.containsKey(moduleName)) {
				ModuleSearchElement newModule = new ModuleSearchElement(moduleName);
				dummies.put(moduleName, newModule);
			}
			dummies.get(moduleName).addElement(match);
		}
		
		List<ModuleSearchElement> moduleList = new ArrayList<ModuleSearchElement>();
		Set<String> keys = dummies.keySet();
		for(String key: keys) {
			moduleList.add(dummies.get(key));
		}
		
		return ((ModuleSearchElement[])moduleList.toArray(new ModuleSearchElement[0]));
	}

}
