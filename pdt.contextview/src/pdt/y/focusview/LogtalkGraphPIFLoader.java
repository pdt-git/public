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
import java.io.IOException;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.ui.util.FileUtils;
import org.eclipse.core.resources.IProject;

import pdt.y.PDTGraphPredicates;
import pdt.y.main.PDTGraphView;

public class LogtalkGraphPIFLoader extends GlobalGraphPIFLoader {
	
	private static final String NAME_OF_DEPENDENCIES_HELPING_FILE = "pdt-logtalk-entity-help.graphml";
	private LogtalkView focusView;
	
	public LogtalkGraphPIFLoader(PDTGraphView view, LogtalkView focusView) {
		super(view, NAME_OF_DEPENDENCIES_HELPING_FILE);
		this.focusView = focusView;
	}
	
	@Override
	protected String generateQuery(File helpFile) {
		try {
			loadPaths(currentPath);

			IProject project = FileUtils.findFileForLocation(currentPath).getProject();
			String projectName = project.getName();
			
			String activeDiagram = focusView.getActiveDiagram();
			
			String query;
			query = bT(PDTGraphPredicates.WRITE_LOGTALK_ENTITIES_TO_GRAPHML, activeDiagram, paths.toString(), Util.quoteAtom(projectName), Util.quoteAtom(Util.prologFileName(helpFile)));
			return query;
			
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	protected boolean ignoreExternalPrologFilesProject() {
		return false;
	}
	
}
