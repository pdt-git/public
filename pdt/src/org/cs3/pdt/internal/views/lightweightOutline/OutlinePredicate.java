package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.List;

import org.cs3.pl.metadata.Predicate;

public class OutlinePredicate extends Predicate{
	private static final long serialVersionUID = 2577159022013132807L;

	private int line;
	//private String file;
	
	public OutlinePredicate(String module, String functor, int arity, List<String> properties, int line){
		super(module, functor, arity, properties);
		this.line = line;
	}

	public int getLine() {
		return line;
	}
}

