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
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;

/**
 * @author gk
 *
 */
public class ReferencesSearchQueryDirect extends PDTSearchQuery {

	
	public ReferencesSearchQueryDirect(Goal goal) {
		super(goal);
		if (goal.isExactMatch()) {
			setSearchType("References to");
		} else {
			setSearchType("References to predicates containing");			
		}
	}


	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		String arity = Integer.toString(goal.getArity());
		if (goal.getArity() < 0) 
			arity = "Arity";
		
		String file = Util.quoteAtom(goal.getFilePath());
		if (goal.getFilePath().isEmpty())
			file = "File";

		String name = Util.quoteAtomIfNeeded(goal.getFunctor());
		if (goal.getFunctor().isEmpty())
			name = "Predicate";
		
		String module2 = module;
		if (module.equals("''"))
			module2 = "Module";
		
		String query = bT(PDTCommonPredicates.FIND_REFERENCE_TO,
				name,
				arity,
				file,
				module2,
				Boolean.toString(goal.isExactMatch()),
				"RefModule",
				"RefName",
				"RefArity",
				"RefFile",
				"RefLine",
				"Nth",
				"Kind",
				"PropertyList");
		return query;
	}

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
			try {
				match = createUniqueMatch(module, name, arity, file, UIUtils.logicalToPhysicalOffset(UIUtils.getDocument(file), offset), length, properties, null, "definition");
			} catch (CoreException e) {
				Debug.report(e);
			}
		} else {
			int line = Integer.parseInt(offsetOrLine);
			match = createUniqueMatch(module, name, arity, file, line, properties, null, "definition");
		}
		return match;
	}
	
	public boolean isCategorized(){
		return false;
	}
	
}


