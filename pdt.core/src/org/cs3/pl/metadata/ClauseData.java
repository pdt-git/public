package org.cs3.pl.metadata;

/**@deprecated*/
public class ClauseData extends PrologElementData implements Clause {

	private static final long serialVersionUID = 1L;

	public ClauseData(String module, String label, int arity, boolean pub,
			boolean dynamic, boolean multifile, SourceLocation knownDefinition) {
		super(module, label, arity, pub, dynamic, multifile, knownDefinition);
	
	}

	public Predicate getPredicate() {
		return new PredicateData(module,label,arity,pub,dynamic,multifile);
	}

	

}
