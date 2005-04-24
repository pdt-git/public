package org.cs3.pl.metadata;

public class PredicateData extends PrologElementData implements Predicate {
	
	private static final long serialVersionUID = 1L;

	public PredicateData(String module,String label, int arity,boolean pub, boolean dynamic, boolean multifile){
		super(module,label, arity,pub, dynamic, multifile);
	}
}
