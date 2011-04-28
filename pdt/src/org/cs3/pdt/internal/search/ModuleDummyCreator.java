package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class ModuleDummyCreator {
	
	public static ModuleSearchDummy[] getModuleDummiesForMatches(List<PrologMatch> matches) {
		if (matches == null) {
			return new ModuleSearchDummy[0];
		}
		Map<String, ModuleSearchDummy> dummies= new HashMap<String, ModuleSearchDummy>();
		for (PrologMatch match:matches) {
			String moduleName = match.getModule();
			
			if (!dummies.containsKey(moduleName)) {
				ModuleSearchDummy newModule = new ModuleSearchDummy(moduleName);
				dummies.put(moduleName, newModule);
			}
			dummies.get(moduleName).addElement(match);
		}
		
		List<ModuleSearchDummy> moduleList = new ArrayList<ModuleSearchDummy>();
		Set<String> keys = dummies.keySet();
		for(String key: keys) {
			moduleList.add(dummies.get(key));
		}
		
		return ((ModuleSearchDummy[])moduleList.toArray(new ModuleSearchDummy[0]));
	}

}
