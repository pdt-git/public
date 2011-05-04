package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pl.metadata.Predicate;

public class OutlinePredicate extends Predicate{
	private static final long serialVersionUID = 2577159022013132807L;

	private int line;
	//private String file;
	
	public OutlinePredicate(String module, String functor, int arity, boolean pub, boolean dynamic, boolean multifile, int line){
		super(module, functor, arity, pub, dynamic, multifile);
		this.line = line;
	}

	int getLine() {
		return line;
	}
}

