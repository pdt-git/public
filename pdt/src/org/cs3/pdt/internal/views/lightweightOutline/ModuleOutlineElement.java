package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.ArrayList;
import java.util.List;

public class ModuleOutlineElement implements PDTTreeElement{
	private String name = "user";
	private List<OutlinePredicate> predicates= new ArrayList<OutlinePredicate>();
	
	public ModuleOutlineElement(String name, List<OutlinePredicate> predicates) {
		this.name = name;
		this.predicates = predicates;
	}
	
	public List<OutlinePredicate> getPredicates() {
		return predicates;
	}
	
	public void update(List<OutlinePredicate> predicates){
		this.predicates = predicates;
	}
	
	public boolean hasChildren() {
		return !(predicates.isEmpty());
	}
	
	public void dispose() {
		predicates.clear();
	}

	@Override
	public Object[] getChildren() {
		return predicates.toArray();
	}

	@Override
	public String getLabel() {
		return name;
	}
}
