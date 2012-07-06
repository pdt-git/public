package org.cs3.pdt.internal.structureElements;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pl.metadata.PrologSourceLocation;

public class OutlineModuleElement 
       extends PrologSourceLocation
       implements PrologTreeElement{
	private String name;  
	private String kind;   // Legal values are "module" (Prolog) or "entity" (Logtalk)
	private Map<String, OutlinePredicate> predicates= new HashMap<String,OutlinePredicate>();
	
	public OutlineModuleElement(String filePath, String name, int line, String kindOfEntity) {
		super(filePath,line);
		this.name = name;
		kind = kindOfEntity;
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
