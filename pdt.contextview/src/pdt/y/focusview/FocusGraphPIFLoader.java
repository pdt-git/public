/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker, Ilshat Aliev
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2013, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.focusview;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.prolog.common.Util;

import pdt.y.PDTGraphPredicates;
import pdt.y.main.PDTGraphView;

public class FocusGraphPIFLoader extends GraphPIFLoaderBase {

	private static final String NAME_OF_FOCUS_HELPING_FILE = "pdt-focus-help.graphml";

	private String focusFile;
	private List<String> dependencies = new ArrayList<String>();

	public FocusGraphPIFLoader(PDTGraphView view) {
		super(view, NAME_OF_FOCUS_HELPING_FILE);
	}

	@Override
	public String getCurrentPath() {
		return focusFile;
	}

	@Override
	public void setCurrentPath(String currentPath) {
		this.focusFile = currentPath;
	}

	public List<String> getDependencies() {
		return dependencies;
	}

	protected String generateQuery(File helpFile) {
		String query;
		query = bT(PDTGraphPredicates.WRITE_FOCUS_TO_GRAPHML, Util.quoteAtom(focusFile), Util.quoteAtom(Util.prologFileName(helpFile)), "Dependencies");
		return query;
	}

	@Override
	public Map<String, Object> loadGraph() {

		Map<String, Object> output = super.loadGraph();

		dependencies.clear();

		if (output != null && output.containsKey("Dependencies")) {
			@SuppressWarnings("unchecked")
			Vector<String> deps = (Vector<String>) output.get("Dependencies");
			dependencies.addAll(deps);
		}
		return output;
	}
}
