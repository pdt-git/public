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
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.progress.UIJob;

import pdt.y.focusview.LogtalkView.DiagramType;
import pdt.y.focusview.LogtalkView.InputType;
import pdt.y.main.PDTGraphView;

public class LogtalkViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	final LogtalkView focusView;
	
	private String lastPath;
	
	public LogtalkViewCoordinator(ViewBase focusView) {
		super(focusView);
		this.focusView = (LogtalkView) focusView;
	}
	
	@Override
	public void swichFocusView(String path) {
		lastPath = path;
		try {
			IProject project = FileUtils.findFileForLocation(path).getProject();
		
			String signature = getSignature(project.getName(), focusView.getDiagramType(), focusView.getInputType(), focusView.getCurrentLibrary());
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
	
	public void diagramSettingsChanged() {
		new UIJob("Update View") {
			@Override
			public IStatus runInUIThread(IProgressMonitor monitor) {
				swichFocusView(lastPath);
				return Status.OK_STATUS;
			}
		}.schedule();
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphPIFLoader)currentFocusView.getPifLoader()).containsFilePath(path);
	}
	
	private String getSignature(String projectName, DiagramType diagramType, InputType inputType, String library) {
		String signature;
		if (inputType == InputType.PROJECT) {
			signature = diagramType.getLabel() + "/" + projectName;
		} else {
			signature = inputType.getLabel() + "/" + diagramType.getLabel() + "/" + library;
		}
		return signature;
	}
}
