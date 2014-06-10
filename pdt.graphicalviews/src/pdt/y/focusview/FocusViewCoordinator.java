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

import java.util.HashMap;

import pdt.y.main.PDTGraphView;

public class FocusViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public FocusViewCoordinator(ViewBase focusView) {
		super(focusView);
	}
	
	public void swichFocusView(String path) {
		currentFocusView = views.get(path);
		if (currentFocusView == null) {
			PDTGraphView pdtGraphView = new PDTGraphView(focusView);
			GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
			loader.setCurrentPath(path);
			
			currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);

			refreshCurrentView();
			
			views.put(path, currentFocusView);
		}
		
		currentFocusView.recalculateMode();
		focusView.setCurrentFocusView(currentFocusView);
	}

	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return views.containsKey(path) && views.get(path) == currentFocusView;
	}
}
