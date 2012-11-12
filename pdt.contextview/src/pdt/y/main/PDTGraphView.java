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

package pdt.y.main;

import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_INDIVIDUAL;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MAXIMUM;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MEDIAN;

import java.awt.BorderLayout;
import java.awt.Cursor;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.net.URL;
import java.util.Arrays;

import javax.swing.JPanel;

import org.eclipse.jface.preference.IPreferenceStore;

import pdt.y.graphml.GraphMLReader;
import pdt.y.model.GraphDataHolder;
import pdt.y.model.GraphLayout;
import pdt.y.model.GraphModel;
import pdt.y.preferences.PredicateLayoutPreferences;
import pdt.y.view.modes.HierarchicPopupMode;
import pdt.y.view.modes.MoveSelectedSelectionMode;
import pdt.y.view.modes.ToggleOpenClosedStateViewMode;
import pdt.y.view.modes.WheelScroller;
import y.base.Node;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.EditMode;
import y.view.Graph2D;
import y.view.Graph2DView;
import y.view.Graph2DViewMouseWheelZoomListener;
import y.view.NavigationMode;
import y.view.ViewMode;

public class PDTGraphView extends  JPanel {
	final Graph2DView view;
	GraphModel model;
	Graph2D graph;
	GraphMLReader reader;

	GraphLayout layoutModel;
	
	EditMode editMode;
	NavigationMode navigationMode;
	Graph2DViewMouseWheelZoomListener wheelZoomListener;
	WheelScroller wheelScroller;

	private static final long serialVersionUID = -611433500513523511L;

	public PDTGraphView()
	{
		setLayout(new BorderLayout());
		
		layoutModel = new  GraphLayout();

		reader = new GraphMLReader();
		view = new Graph2DView();

		initEditMode();
		
		initNavigationMode();
		
		initMouseZoomSupport();

		initKeyListener();
		
		add(view);
	}

	private void  initEditMode() {
		editMode = new EditMode();
		editMode.allowNodeCreation(false);
		editMode.allowEdgeCreation(false);
		editMode.setPopupMode(new HierarchicPopupMode());
		editMode.setMoveSelectionMode(new MoveSelectedSelectionMode(new OrthogonalEdgeRouter()));
		
		view.addViewMode(editMode);
		view.addViewMode(new ToggleOpenClosedStateViewMode());
		
	}

	protected void initNavigationMode() {
		navigationMode = new NavigationMode();
		navigationMode.setDefaultCursor(new Cursor(Cursor.MOVE_CURSOR));
		navigationMode.setNavigationCursor(new Cursor(Cursor.MOVE_CURSOR));
	}

	private void initMouseZoomSupport() {
		wheelZoomListener = new Graph2DViewMouseWheelZoomListener();
		wheelScroller = new WheelScroller(view);
		
		view.getCanvasComponent().addMouseWheelListener(wheelScroller);
	}
	
	private void initKeyListener() {
		view.getCanvasComponent().addKeyListener(new KeyListener() {
			
			Boolean navigation = false;
			
			@Override
			public void keyPressed(KeyEvent e) {
				if (navigation)
					return;
				
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					navigationMode();
					navigation = true;
				}
			}

			@Override
			public void keyReleased(KeyEvent e) {
				if (!navigation)
					return;
				
				if (e.getKeyCode() == KeyEvent.VK_CONTROL) {
					editMode();
					navigation = false;
				}
			}

			@Override
			public void keyTyped(KeyEvent arg0) { }
		});
	}
	
	public void navigationMode() {
		view.removeViewMode(editMode);
		view.addViewMode(navigationMode);
		
		view.getCanvasComponent().removeMouseWheelListener(wheelScroller);
		view.getCanvasComponent().addMouseWheelListener(wheelZoomListener);
	}
	
	public void editMode() {
		view.removeViewMode(navigationMode);
		view.addViewMode(editMode);
		
		view.getCanvasComponent().removeMouseWheelListener(wheelZoomListener);
		view.getCanvasComponent().addMouseWheelListener(wheelScroller);
	}

	public GraphDataHolder getDataHolder() {
		return model.getDataHolder();
	}
	
	public Graph2D getGraph2D() {
		return graph;
	}

	public void addViewMode(ViewMode viewMode){
		view.addViewMode(viewMode);
	}

	public void setModel(GraphModel model){
		this.model = model;
	}

	public void loadGraph(URL resource) {
		model = reader.readFile(resource);
		model.categorizeData();
		model.assignPortsToEdges();
		graph = model.getGraph();
		view.setGraph2D(graph);

		updateView();
	}

	private void updateView() {
		
		Graphics gfx = view.getGraphics();
		FontMetrics fontmtx = gfx.getFontMetrics(gfx.getFont());
		
		int i = 0;
		int[] lengths = new int[graph.getNodeArray().length];
		
		
		for (Node node: graph.getNodeArray()) {
			String text = createFirstLabel(node);
			
			int v = (int)(fontmtx.getStringBounds(text, gfx).getWidth() + 14);
			lengths[i++] = v;
		}
		
		Arrays.sort(lengths);
		
		for (Node node: graph.getNodeArray()) {
			initializeBoxSize(node, lengths[i - 1], lengths[i / 2], gfx, fontmtx);
		}
		calcLayout();
	}

	public boolean isEmpty() {
		return graph == null 
			|| graph.getNodeArray().length == 0;
	}

	private String createFirstLabel(Node node) {
		String labelText = model.getLabelTextForNode(node);
		graph.setLabelText(node,labelText);
		return labelText;
	}

	protected void initializeBoxSize(Node node, int maximumValue, int medianValue, Graphics gfx, FontMetrics fontmtx) {
		
		int width = 0;
		int height = PredicateLayoutPreferences.getNumberOfLines() * 16 + 20;
		
		IPreferenceStore prefs = PredicateLayoutPreferences.getCurrentPreferences();
		
		
		if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_FIXED)) {
			width = prefs.getInt(NODE_SIZE_FIXED_WIDTH);
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MAXIMUM)) {
			width = maximumValue;
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MEDIAN)) {
			width = medianValue;
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_INDIVIDUAL)) {
			width = (int)fontmtx.getStringBounds(model.getLabelTextForNode(node), gfx).getWidth() + 14;
		}
		
		graph.setSize(node, width, height);
	}

	public void calcLayout() {
		view.applyLayout(layoutModel.getLayouter());
		
		//		layoutModel.getLayouter().doLayout(graph);
		//		graph.updateViews();
		
		view.fitContent();
		view.updateView();
	}
	
	public void updateLayout() {
		view.applyLayoutAnimated(layoutModel.getLayouter());
		view.fitContent();
		view.updateView();
	}
}


