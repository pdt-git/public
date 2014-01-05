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

import java.util.ArrayList;

import org.cs3.prolog.common.logging.Debug;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.swt.widgets.Composite;

import pdt.y.main.PDTGraphView;


public class LogtalkView extends ViewBase {
	
	public static final String ENTITY_DIAGRAM = "entity_diagram";
	public static final String ENTITY_DIAGRAM_LABEL = "Entity diagram";
	
	public static final String CALL_DIAGRAM = "call_diagram";
	public static final String CALL_DIAGRAM_LABEL = "Call diagram";
	
	public static final String INHERITANCE_DIAGRAM = "inheritance_diagram";
	public static final String INHERITANCE_DIAGRAM_LABEL = "Inheritance diagram";
	
	public static final String FILE_DIAGRAM = "file_diagram";
	public static final String FILE_DIAGRAM_LABEL = "File diagram";

	public static final String USES_DIAGRAM = "uses_diagram";
	public static final String USES_DIAGRAM_LABEL = "Uses diagram";
	
	private String activeDiagram = ENTITY_DIAGRAM;
	private String activeDiagramLabel = ENTITY_DIAGRAM_LABEL;
	
	private ArrayList<SelectDiagramTypeAction> diagramSelectors = new ArrayList<SelectDiagramTypeAction>(5);
	private LogtalkViewCoordinator logtalkViewCoordinator;
	private String statusText = "";
	
	@Override
	public GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new LogtalkGraphPIFLoader(pdtGraphView, this);
	}

	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		logtalkViewCoordinator = new LogtalkViewCoordinator(this);
		return logtalkViewCoordinator;
	}
	
	@Override
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		try {
			SelectDiagramTypeAction action;
			IMenuManager menuManager = getViewSite().getActionBars().getMenuManager();

			action = new SelectDiagramTypeAction(ENTITY_DIAGRAM, ENTITY_DIAGRAM_LABEL);
			diagramSelectors.add(action);
			menuManager.add(action);
			
			action = new SelectDiagramTypeAction(CALL_DIAGRAM, CALL_DIAGRAM_LABEL);
			diagramSelectors.add(action);
			menuManager.add(action);
			
			action = new SelectDiagramTypeAction(INHERITANCE_DIAGRAM, INHERITANCE_DIAGRAM_LABEL);
			diagramSelectors.add(action);
			menuManager.add(action);
			
			action = new SelectDiagramTypeAction(FILE_DIAGRAM, FILE_DIAGRAM_LABEL);
			diagramSelectors.add(action);
			menuManager.add(action);
			
			action = new SelectDiagramTypeAction(USES_DIAGRAM, USES_DIAGRAM_LABEL);
			diagramSelectors.add(action);
			menuManager.add(action);
		} catch (Exception e) {
			Debug.report(e);
		}
	}
	
	private class SelectDiagramTypeAction extends Action {
		
		private final String type;
		private final String label;

		private SelectDiagramTypeAction(String type, String label) {
			super(label, AS_RADIO_BUTTON);
			this.type = type;
			this.label = label;
			updateChecked();
		}
		
		private void updateChecked() {
			setChecked(type == LogtalkView.this.activeDiagram);
		}
		
		@Override
		public void run() {
			if (LogtalkView.this.activeDiagram == type) {
				return;
			}
			LogtalkView.this.activeDiagram = type;
			LogtalkView.this.activeDiagramLabel = label;
			for (SelectDiagramTypeAction action : LogtalkView.this.diagramSelectors) {
				if (action != this) {
					action.updateChecked();
				}
			}
			LogtalkView.this.logtalkViewCoordinator.diagramTypeChanged();
			LogtalkView.this.updateStatusText();
		}
	}
	
	String getActiveDiagram() {
		return activeDiagram;
	}
	
	@Override
	public void setStatusText(String text) {
		this.statusText = text;
		super.setStatusText(activeDiagramLabel + " " + text);
	}
	
	private void updateStatusText() {
		super.setStatusText(activeDiagramLabel + " " + statusText);
	}
	
}
