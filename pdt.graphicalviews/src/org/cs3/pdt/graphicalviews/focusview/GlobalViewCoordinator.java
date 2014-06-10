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

package org.cs3.pdt.graphicalviews.focusview;

import java.io.IOException;
import java.util.HashMap;

import org.cs3.pdt.graphicalviews.main.PDTGraphView;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.ui.util.FileUtils;
import org.eclipse.core.resources.IProject;

public class GlobalViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public GlobalViewCoordinator(ViewBase focusView)
	{
		super(focusView);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
		
	public void swichFocusView(String path) {
		try {
			IProject project = FileUtils.findFileForLocation(path).getProject();
		
			currentFocusView = views.get(project.getName());
			
			if (currentFocusView == null) {
				PDTGraphView pdtGraphView = new PDTGraphView(focusView);
				GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
				loader.setCurrentPath(path);
				
				currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);
	
				refreshCurrentView();
				
				views.put(project.getName(), currentFocusView);
			}
	
			currentFocusView.recalculateMode();
			focusView.setCurrentFocusView(currentFocusView);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphPIFLoader)currentFocusView.getPifLoader()).containsFilePath(path);
	}
}
