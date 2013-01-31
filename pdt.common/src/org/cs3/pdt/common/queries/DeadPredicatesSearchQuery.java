/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
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
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.text.Match;

public class DeadPredicatesSearchQuery extends MarkerCreatingSearchQuery {

	private static final String ATTRIBUTE = "pdt.dead.predicate";

	public DeadPredicatesSearchQuery(boolean createMarkers) {
		super(new Goal("", "", "", -1, ""), createMarkers, ATTRIBUTE, ATTRIBUTE);
		setSearchType("Dead predicates");
	}

	@Override
	protected String buildSearchQuery(Goal goal, String module) {
		return bT(PDTCommonPredicates.FIND_DEAD_PREDICATE, "Module", "Name", "Arity", "File", "Location", "PropertyList");
	}

	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String definingModule = m.get("Module").toString();
		String functor = m.get("Name").toString();
		int arity=-1;
		try {
			arity = Integer.parseInt(m.get("Arity").toString());
		} catch (NumberFormatException e) {}
		
		IFile file = findFile(m.get("File").toString());
		String offsetOrLine = m.get("Location").toString();

		Object prop = m.get("PropertyList");
		List<String> properties = null;
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}	
		Match match = null;
		if (offsetOrLine.indexOf("-") >= 0) {
			String[] positions = offsetOrLine.split("-");
			int offset = Integer.parseInt(positions[0]);
			int end = Integer.parseInt(positions[1]);
			match = createUniqueMatch(definingModule, functor, arity, file, offset, end - offset, properties, "", "definition");
			if (createMarkers && match != null) {
				try {
					IDocument document = UIUtils.getDocument(file);
					offset = UIUtils.logicalToPhysicalOffset(document, offset);
					end = UIUtils.logicalToPhysicalOffset(document, end);
					createMarker(file, definingModule + ":" + functor + "/" + arity + " is dead", offset, end);
				} catch (CoreException e) {
					Debug.report(e);
				}
			}
		} else {
			int line = Integer.parseInt(offsetOrLine);
			match = createUniqueMatch(definingModule, functor, arity, file, line, properties, "", "definition");
			if (createMarkers && match != null) {
				try {
					createMarker(file, "Dead predicate: " + definingModule + ":" + functor + "/" + arity, line);
				} catch (CoreException e) {
					Debug.report(e);
				}
			}
		}
		
		return match;
	}

}
