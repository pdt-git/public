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
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.search.ui.text.Match;

public class UndefinedCallsSearchQuery extends MarkerCreatingSearchQuery {
	
	private static final String ATTRIBUTE = "pdt.undefined.call";
	
	private IProject root;
	private String rootPath;

	public UndefinedCallsSearchQuery(boolean createMarkers) {
		this(createMarkers, null);
	}

	public UndefinedCallsSearchQuery(boolean createMarkers, IProject root) {
		super(createMarkers, ATTRIBUTE, ATTRIBUTE);
		this.root = root;
		if (root == null) {
			setSearchType("Undefined calls");
		} else {
			setSearchType("Undefined calls in project " + root.getName());
			rootPath = Util.quoteAtom(Util.prologFileName(root.getLocation().toFile()));
		}
	}

	@Override
	protected String buildSearchQuery() {
		return bT(PDTCommonPredicates.FIND_UNDEFINED_CALL,
				rootPath == null ? "_" : rootPath,
				"Module",
				"Name",
				"Arity",
				"File",
				"Start",
				"End",
				"UndefName",
				"UndefArity",
				"PropertyList");
	}
	
	@SuppressWarnings("unchecked")
	@Override
	protected Match constructPrologMatchForAResult(Map<String, Object> m) throws IOException {
		String module = m.get("Module").toString();
		String name = m.get("Name").toString();
		int arity = Integer.parseInt(m.get("Arity").toString());
		
		List<String> properties = null;
		Object prop = m.get("PropertyList");
		if (prop instanceof Vector<?>) {
			properties = (Vector<String>)prop;
		}
		IFile file = findFile(m.get("File").toString());
		
		if (root != null && !root.equals(file.getProject())) {
			return null;
		}

		int offset = Integer.parseInt(m.get("Start").toString());
		int end = Integer.parseInt(m.get("End").toString());
		PrologMatch match = createUniqueMatch(PROLOG_MATCH_KIND_REFERENCE, module, name, arity, file, offset, end - offset, properties, null, "definition");
		if (createMarkers && match != null) {
			try {
				IDocument document = UIUtils.getDocument(file);
				offset = UIUtils.logicalToPhysicalOffset(document, offset);
				end = UIUtils.logicalToPhysicalOffset(document, end);
				createMarker(file, "Undefined call: " + m.get("UndefName") + "/" + m.get("UndefArity") + " is not defined", offset, end);
			} catch (CoreException e) {
				Debug.report(e);
			}
		}
		return match;
	}
	
}
