package org.cs3.pdt.internal.editors;

public class PrologPredicate {
	String name;
	int arity;
	int line;
	String file;
	
	@Override
	public String toString() {
		return name + "/" + arity;
	}
}

