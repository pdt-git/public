package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.Map;


public class PrologSourceFileModel {

	private Map<String, ModuleElement> modules;
	
	public PrologSourceFileModel(Map<String, ModuleElement> modules) {
		this.modules = modules;
	}
	
	public void update(Map<String,ModuleElement> modules){
		this.modules = modules;
	}
	
	public boolean hasChildren() {
		if((modules == null) || (modules.isEmpty()))
			return false;
		return true;
	}

	public Object[] getElements() {
		return modules.values().toArray();
	}
	
	public void dispose() {
		modules.clear();
	}
}
