package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.HashMap;
import java.util.Map;

public class ModuleElement implements PDTTreeElement{
	private String name;
	private String kind;
	private Map<String, OutlinePredicate> predicates= new HashMap<String,OutlinePredicate>();
	
	public ModuleElement(String name, String kindOfEntity) {
		this.name = name;
	}
	
	public boolean hasPredicate(String key) {
		return predicates.containsKey(key);
	}
	
	public OutlinePredicate getPredicate(String key) {
		return predicates.get(key);
	}
	
	public void addChild(String key, OutlinePredicate predicate) {
		predicates.put(key, predicate);
	}
	
	public boolean hasChildren() {
		return !(predicates.isEmpty());
	}
	
	public String getKind() {
		return kind;
	}
	
	public void dispose() {
		predicates.clear();
	}

	@Override
	public Object[] getChildren() {
		return predicates.values().toArray();
	}

	@Override
	public String getLabel() {
		return name;
	}
}
