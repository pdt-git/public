/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.queries;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.metadata.Goal;
import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class GlobalDefinitionsSearchQuery extends PDTSearchQuery {
	public GlobalDefinitionsSearchQuery(Goal goal) {
		super(goal);
		if (goal.isExactMatch()) {
			setSearchType("Definitions and declarations of");
		} else {
			setSearchType("Definitions and declarations containing");			
		}
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		String term;
		
		if (goal.getArity() == -1) {
			term = goal.getFunctor();
		} else {
			term = goal.getSignature();
		}
		
		String query = bT(PDTCommonPredicates.FIND_DEFINITIONS_CATEGORIZED,
				term,
				Boolean.toString(goal.isExactMatch()),
				"DefiningModule",
				"Functor",
				"Arity",
				"DeclOrDef",
				"File",
				"Line",
				"PropertyList");
		return query;
	}

	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String definingModule = m.get("DefiningModule").toString();
		String functor = m.get("Functor").toString();
		int arity=-1;
		try {
			arity = Integer.parseInt(m.get("Arity").toString());
		} catch (NumberFormatException e) {}
		
		IFile file = findFile(m.get("File").toString());
		int line = Integer.parseInt(m.get("Line").toString());

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		String declOrDef = m.get("DeclOrDef").toString();

		Match match;
		if (file == null) {
			match = createUniqueMatch(definingModule, functor, arity, properties, "", declOrDef);
		} else {
			match = createUniqueMatch(definingModule, functor, arity, file, line, properties, "", declOrDef);
		}
		
		return match;
	}
	
}


