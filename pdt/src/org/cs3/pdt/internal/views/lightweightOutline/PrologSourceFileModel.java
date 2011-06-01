package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.List;


public class PrologSourceFileModel {

	private List<ModuleOutlineElement> modules;
	
	public PrologSourceFileModel(List<ModuleOutlineElement> modules) {
		this.modules = modules;
	}
	
	public void update(List<ModuleOutlineElement> modules){
		this.modules = modules;
	}
	
	public boolean hasChildren() {
		if((modules == null) || (modules.isEmpty()))
			return false;
		return true;
	}

	public Object[] getElements() {
		return modules.toArray();
	}
	
	public void dispose() {
		for (ModuleOutlineElement module : modules) {
				module.dispose();;
		}
	}
}
