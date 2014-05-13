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

package pdt.y.model.realizer.nodes;

import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_FIXED_WIDTH;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_INDIVIDUAL;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MAXIMUM;
import static pdt.y.preferences.PreferenceConstants.NODE_SIZE_MEDIAN;

import java.awt.Color;
import java.awt.Graphics2D;

import org.eclipse.jface.preference.IPreferenceStore;

import pdt.y.model.GraphDataHolder;
import pdt.y.model.GraphModel;
import pdt.y.preferences.PredicateAppearancePreferences;
import pdt.y.preferences.PredicateLayoutPreferences;
import pdt.y.utils.Size;
import y.base.Node;
import y.geom.YDimension;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;


public class PredicateNodeRealizer extends NodeRealizerBase {

	public static final int INITIAL_STATE = 0;
	public static final int TRANSITION_STATE = 1;
	public static final int FINAL_STATE = 2;
	public static final byte CUSTOM_SHAPE = 4;
	protected final double MAX_NODE_HEIGHT = Double.MAX_VALUE;
	protected final double MAX_NODE_WIDTH = Double.MAX_VALUE;
	private int state;
	private GraphModel	 model;

	public PredicateNodeRealizer(GraphModel  model){
		super(ShapeNodeRealizer.ROUND_RECT);
		state = TRANSITION_STATE;
		this.model= model;

		setFillColor(Color.WHITE);
	}

	public PredicateNodeRealizer(NodeRealizer r)
	{
		super(r);
		if(r instanceof PredicateNodeRealizer)
		{
			PredicateNodeRealizer sr = (PredicateNodeRealizer)r;
			state = sr.state;
			model	= sr.model;
		}
		else
		{
			state = FINAL_STATE;
		}
		
		init();
	}

	protected void init() {
		
		NodeLabel label = getLabel();
		
		label.setConfiguration(PredicateLayoutPreferences.getNameCroppingConfiguration());
		label.setAutoSizePolicy(NodeLabel.AUTOSIZE_NODE_SIZE);
		
		label.setUserData(new YDimension(getWidth(), getHeight()));
	}
	
	@Override
	protected void paintNode(Graphics2D gfx) {
		byte myStyle;
		if (model.getDataHolder().isDynamicNode(getNode())) {
			myStyle = PredicateAppearancePreferences.getDynamicPredicateBorderStyle().getLineStyle();
		} else if ("inferred".equals(model.getDataHolder().getMetaPredType(getNode()))) {
			myStyle = LineType.DASHED_2.getLineStyle();
		} else {
			myStyle = PredicateAppearancePreferences.getBorderStyle().getLineStyle();
		}
		
//		if ("inferred".equals(model.getDataHolder().getMetaPredType(getNode()))) {
//			setLabelText(model.getLabelTextForNode(getNode()) + " [i]");
//		}
		
		LineType myLineType = LineType.getLineType(1, myStyle);
		setLineType(myLineType);

		if (model.getDataHolder().isMetaPred(getNode())) {
			setShapeType(HEXAGON);
		} else if (model.getDataHolder().isTransparentNode(getNode())) {
			setShapeType(ELLIPSE);
		} else {
			setShapeType(ROUND_RECT);
		}

		if (model.getDataHolder().isExported(getNode())) {
			setFillColor(PredicateAppearancePreferences.getExportedPredicateColor());
		} else {
			setFillColor(PredicateAppearancePreferences.getPredicateColor());
		}

		if (model.getDataHolder().isUnusedLocal(getNode())) {
			setLineColor(PredicateAppearancePreferences.getUnusedPredicateBorderColor());
		} else {
			setLineColor(PredicateAppearancePreferences.getBorderColor());
		}
		
		super.paintNode(gfx);
	}
	
	public void fitContent() {
		
		Size s = calcLabelSize(model.getLabelTextForNode(getNode()));
		
		int width = 0;
		int height = PredicateLayoutPreferences.getNumberOfLines() * s.getHeight() + 20;
		
		IPreferenceStore prefs = PredicateLayoutPreferences.getCurrentPreferences();
		
		if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_FIXED)) {
			width = prefs.getInt(NODE_SIZE_FIXED_WIDTH);
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MAXIMUM)) {
			width = model.getNodesMaxWidth();
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_MEDIAN)) {
			width = model.getNodesMedianWidth();
		}
		else if (PredicateLayoutPreferences.getNodeSizePreference().equals(NODE_SIZE_INDIVIDUAL)) {
			width = (int)s.getWidth() + 14;
		}
		
		setSize(width, height);
	}
	
	@Override
	public String getInfoText() {
		StringBuilder sb = new StringBuilder(); 
		Node node = getNode();

		GraphDataHolder data = model.getDataHolder();

		if (data.isModule(node)) {
			sb.append("Module: ");
		} else if (data.isFile(node)) {
			sb.append("File: ");
		} else if (data.isPredicate(node)) {
			sb.append("Predicate: ");
		}

		sb.append(getLabelText());
		
		if (data.isExported(node)) {
			sb.append(" [Exported]");
		}

		if (data.isDynamicNode(node)) {
			sb.append(" [Dynamic]");
		}

		if (data.isUnusedLocal(node)) {
			sb.append(" [Unused]");
		}
		
		return sb.toString();
	}

	public int getState() {
		return state;
	}

	public void setState(int initialState) {
		state  = initialState;
	}

	@Override
	protected void labelBoundsChanged(NodeLabel arg0) {
		getLabel().setUserData(new YDimension(getWidth(), getHeight()));
		
		super.labelBoundsChanged(arg0);
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer r)
	{
		return new PredicateNodeRealizer(r);
	}

}


