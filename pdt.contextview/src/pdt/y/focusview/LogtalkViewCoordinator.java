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

import java.io.IOException;
import java.util.HashMap;

import org.cs3.prolog.ui.util.FileUtils;
import org.eclipse.core.resources.IProject;

import pdt.y.main.PDTGraphView;

public class LogtalkViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	final LogtalkView focusView;
	
	private String lastPath;
	
	public LogtalkViewCoordinator(ViewBase focusView) {
		super(focusView);
		this.focusView = (LogtalkView) focusView;
	}
	
	public void swichFocusView(String path) {
		lastPath = path;
		try {
			IProject project = FileUtils.findFileForLocation(path).getProject();
		
			String signature = project.getName() + "/" + focusView.getActiveDiagram();
			currentFocusView = views.get(signature);
			
			if (currentFocusView == null) {
				PDTGraphView pdtGraphView = new PDTGraphView(focusView);
				GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
				loader.setCurrentPath(path);
				
				currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);
	
				refreshCurrentView();
				
				views.put(signature, currentFocusView);
			}
	
			currentFocusView.recalculateMode();
			focusView.setCurrentFocusView(currentFocusView);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void diagramTypeChanged() {
		swichFocusView(lastPath);
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphPIFLoader)currentFocusView.getPifLoader()).containsFilePath(path);
	}
}
