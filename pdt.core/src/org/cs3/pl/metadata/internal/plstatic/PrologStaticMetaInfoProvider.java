/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

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


