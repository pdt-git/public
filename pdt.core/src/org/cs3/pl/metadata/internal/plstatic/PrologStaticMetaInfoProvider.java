/* $LICENSE_MSG$(ld) */

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
	
	//TODO: check if this class is dead code
	
	@Override
	public Clause[] findClauses(Predicate p) {
		return null;
	}

	@Override
	public Predicate[] getPredicatesWithPrefix(String module, String prefix,
			String activeFileName) throws PrologException {
		return null;
	}

	@Override
	public Predicate[] getPredicatesWithPrefix(String string, String prefix)
			throws NumberFormatException, PrologException {
		return null;
	}

	@Override
	public Clause[] retrievePrologElements(String filename)
			throws PrologException {
		return null;
	}

	@Override
	public String getHelp(Predicate elm) {
		return null;
	}

	@Override
	public SourceLocation[] findReferences(Predicate data) {
		return null;
	}

	@Override
	public Predicate[] findPredicates(Goal data) {
		return null;
	}

	@Override
	public String getSummary(Predicate data) throws PrologException {
		return null;
	}

}

