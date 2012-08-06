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
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.prolog.common.FileUtils;
import org.eclipse.core.resources.IFile;

public class DefinitionsSearchQuery extends PDTSearchQuery {
	public DefinitionsSearchQuery(Goal goal) {
		super(goal);
		setSearchType("Definitions and declarations of");
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {		
		String file = "'"+goal.getFilePath()+"'";
		if (goal.getFilePath().equals(""))
			file = "OrigFile";

		String module2 = module;
		if (module.equals("''"))
			module2 = "Module";
		
		String term = goal.getTermString();
		//String term = Util.quoteAtom(origTerm);
		
		
		String query = bT(PDTCommonPredicates.FIND_DEFINITIONS_CATEGORIZED,
				file,
				goal.getLine(),
				term,
				"Functor",
				"Arity",
				module2,
				"DeclOrDef",
				"DefiningModule",
				"File",
				"Line",
				"PropertyList",
				"Visibility");
		return query;
	}



	@SuppressWarnings("unchecked")
	@Override
	protected PrologMatch constructPrologMatchForAResult(Map<String, Object> m)
	throws IOException {
		String definingModule = m.get("DefiningModule").toString();
		String functor = m.get("Functor").toString();
		int arity = Integer.parseInt(m.get("Arity").toString());
		IFile file = FileUtils.findFileForLocation(m.get("File").toString());
		int line = Integer.parseInt(m.get("Line").toString());

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		String declOrDef = m.get("DeclOrDef").toString();
		String visibility = m.get("Visibility").toString();

		PrologMatch match = createUniqueMatch(definingModule, functor, arity,
				file, line, properties, visibility, declOrDef);
		
		return match;
	}
	
}


