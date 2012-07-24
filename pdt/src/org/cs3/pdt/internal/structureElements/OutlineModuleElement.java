/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.structureElements;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pdt.metadata.PrologSourceLocation;

public class OutlineModuleElement extends PrologSourceLocation implements PrologOutlineTreeElement{
	private String name;  
	private String kind;   // Legal values are "module" (Prolog) or "entity" (Logtalk)
	private Map<String, OutlinePredicateElement> predicates= new HashMap<String,OutlinePredicateElement>();
	private Object parent;
	
	public OutlineModuleElement(String filePath, String name, int line, String kindOfEntity) {
		super(filePath,line);
		this.name = name;
		kind = kindOfEntity;
	}
	
	public boolean hasPredicate(String key) {
		return predicates.containsKey(key);
	}
	
	public OutlinePredicateElement getPredicate(String key) {
		return predicates.get(key);
	}
	
	public void addChild(String key, OutlinePredicateElement predicate) {
		predicates.put(key, predicate);
	}
	
	@Override
	public boolean hasChildren() {
		return !(predicates.isEmpty());
	}
	
	public String getKind() {
		return kind;
	}
	
	@Override
	public Object[] getChildren() {
		return predicates.values().toArray();
	}

	@Override
	public String getLabel() {
		return name;
	}

	@Override
	public int hashCode() {
		return name.hashCode();
	}
	
	@Override
	public boolean equals(Object object) {
		if (object == null || !(object instanceof OutlineModuleElement)) {
			return false;
		} else {
			OutlineModuleElement other = (OutlineModuleElement) object;
			return (name.equals(other.name) && kind.equals(other.kind));
		}
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}

	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public void addClause(PrologClause clause) {
		String signature = getSignature(clause);
		OutlinePredicateElement predicateElement = predicates.get(signature);
		if (predicateElement == null) {
			predicateElement = new OutlinePredicateElement(this, clause.getEntity(), clause.getFunctor(), clause.getArity(), clause.getProperties(), clause.getFile());
			predicates.put(signature, predicateElement);
		}
		predicateElement.addClause(clause);
	}
	
	private String getSignature(PrologClause clause) {
		return clause.getFunctor() + "/" + clause.getArity();
	}

}

