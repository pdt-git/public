package org.cs3.pdt.internal.views;

import java.util.HashMap;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Predicate;

public class PredicateNode implements Predicate {

	HashMap properties = new HashMap();

	String name;

	int arity;

	private String module;

	public PredicateNode(CTerm signature, String defaultModule) {
		CCompound term = (CCompound) signature;
		module=defaultModule;
		if(":".equals(term.getFunctorValue())){
			module=term.getArgument(0).getFunctorValue();
			term=(CCompound) term.getArgument(1);
		}
		name=term.getArgument(0).getFunctorValue();
		arity=((CInteger)term.getArgument(1)).getIntValue();
	}
	
	public PredicateNode(String module, String name, int arity) {
		this.module = module;
		this.name = name;
		this.arity = arity;
	}

	public String getPredicateProperty(String property) {
		return (String) properties.get(property);
	}

	public String getSignature() {
		return getName() + "/" + getArity();
	}

	public String getName() {
		return name;
	}

	public int getArity() {
		return arity;
	}

	public String getModule() {
		return module;
	}

	public boolean isDynamic() {
		return "true".equals(getPredicateProperty(DYNAMIC));
	}

	public boolean isMultifile() {
		return "true".equals(getPredicateProperty(MULTIFILE));
	}

	public boolean isPublic() {
		return "user".equals(getModule())
				|| "true".equals(getPredicateProperty(EXPORTED));
	}

	public boolean equals(Object obj) {
		if (!(obj instanceof Predicate)) {
			return false;
		}
		Predicate other = (Predicate) obj;
		return getModule().equals(other.getModule())
				&& getName().equals(other.getName())
				&& getArity() == other.getArity();
	}
	
	public int hashCode() {
	
		return module.hashCode()+name.hashCode()+arity;
	}

	public int compareTo(Object arg0) {
		if(!(arg0 instanceof Predicate)){
			return -1;
		}
		Predicate other = (Predicate) arg0;
		int c = getModule().compareTo(other.getModule());
		if(c!=0){
			return c;
		}
		c = getName().compareTo(other.getName());
		if(c!=0){
			return c;
		}
		c = getArity()-other.getArity();
		
		return c;
	}

	public void setPredicateProperty(String property, String value) {
		properties.put(property, value);
		
	}
}
