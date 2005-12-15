package org.cs3.pl.metadata.internal.plstatic;

import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;

public class PrologStaticMetaInfoProvider implements IMetaInfoProvider {

	PrologInterface pif;
	
	public Clause[] findClauses(Predicate p) {
		// TODO Auto-generated method stub
		return null;
	}

	public Predicate[] getPredicatesWithPrefix(String module, String prefix,
			String activeFileName) throws PrologException {
		// TODO Auto-generated method stub
		return null;
	}

	public Predicate[] getPredicatesWithPrefix(String string, String prefix)
			throws NumberFormatException, PrologException {
		// TODO Auto-generated method stub
		return null;
	}

	public Clause[] retrievePrologElements(String filename)
			throws PrologException {
		// TODO Auto-generated method stub
		return null;
	}

	public String getHelp(Predicate elm) {
		// TODO Auto-generated method stub
		return null;
	}

	public SourceLocation[] findReferences(Predicate data) {
		// TODO Auto-generated method stub
		return null;
	}

	public Predicate[] findPredicates(Goal data) {
		// TODO Auto-generated method stub
		return null;
	}

	public String getSummary(Predicate data) throws PrologException {
		// TODO Auto-generated method stub
		return null;
	}

}
