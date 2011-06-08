package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.Map;
import org.cs3.pdt.internal.structureElements.OutlineModuleElement;


public class PrologSourceFileModel {

	private Map<String, OutlineModuleElement> modules;
	
	public PrologSourceFileModel(Map<String, OutlineModuleElement> modules) {
		this.modules = modules;
	}
	
	public void update(Map<String,OutlineModuleElement> modules){
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
