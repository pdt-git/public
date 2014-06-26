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

import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.prolog.connector.common.QueryUtils;
import org.eclipse.core.resources.IFile;

/**
 * @author gk
 *
 */
public class ReferencesSearchQueryDirect extends PDTSearchQuery {

	private String filePath;
	private int line = -1;
	
	public ReferencesSearchQueryDirect(String goal, String searchGoalLabel, boolean isExactMatch) {
		super(goal, searchGoalLabel, isExactMatch);
		if (isExactMatch) {
			setSearchType("References to");
		} else {
			setSearchType("References to predicates containing");			
		}
	}
	
	public ReferencesSearchQueryDirect(Goal goal) {
		super(PDTSearchQuery.toPredicateGoal(goal), goal.getTermString(), true);
		setSearchType("References to");
		filePath = QueryUtils.quoteAtomIfNeeded(goal.getFilePath());
		line = goal.getLine();
	}


	@Override
	protected String buildSearchQuery() {
//		String arity = Integer.toString(goal.getArity());
//		if (goal.getArity() < 0) 
//			arity = "Arity";
//		
//		String file = Util.quoteAtom(goal.getFilePath());
//		if (goal.getFilePath().isEmpty())
//			file = "File";
//
//		String name = Util.quoteAtomIfNeeded(goal.getFunctor());
//		if (goal.getFunctor().isEmpty())
//			name = "Predicate";
//		
//		String module2 = module;
//		if (module.equals("''"))
//			module2 = "Module";
//		
		String query = bT(PDTCommonPredicates.FIND_PREDICATE_REFERENCE,
				getGoal(),
//				name,
//				arity,
//				file,
//				module2,
				Boolean.toString(isExactMatch()),
				"RefModule",
				"RefName",
				"RefArity",
				"RefFile",
				"RefLine",
				"PropertyList");
		return query;
	}

	@SuppressWarnings("unchecked")
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {

		String module = m.get("RefModule").toString();
		String name = m.get("RefName").toString();
		int arity = Integer.parseInt(m.get("RefArity").toString());
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		IFile file = findFile(m.get("RefFile").toString());
		String offsetOrLine = m.get("RefLine").toString();
		
		PrologMatch match = null;
		
		if (offsetOrLine.indexOf("-") >= 0) {
			String[] positions = offsetOrLine.split("-");
			int offset = Integer.parseInt(positions[0]);
			int length = Integer.parseInt(positions[1]) - offset;
			match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, offset, length, properties, null, "definition");
		} else {
			int line = Integer.parseInt(offsetOrLine);
			match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, line, properties, null, "definition");
		}
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}


