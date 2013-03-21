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

package pdt.y.model.realizer.edges;

import pdt.y.model.GraphModel;
import pdt.y.preferences.EdgeAppearancePreferences;
import y.base.Edge;
import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.LineType;

public class CallEdgeRealizer extends EdgeRealizerBase {

	private static final float METER_LIMIT = (float)1.45;
	private static final float DASH_PHASE = (float)0.0;
	private float width;

	public CallEdgeRealizer() {
		super();
		init();
	}
	
	public CallEdgeRealizer(EdgeRealizer realizer){
		super(realizer);
		init();
	}
	
	private void init() {
		setTargetArrow(Arrow.PLAIN);
		setLineColor(EdgeAppearancePreferences.getLineColor());
		LineType myLineType = LineType.createLineType(1, LineType.CAP_ROUND, LineType.JOIN_ROUND, (float) METER_LIMIT, null, DASH_PHASE);
		setLineType(myLineType);
	}
	
	private int calculateLineWidth(GraphModel model) {
		Edge edge = getEdge();
		if(model.isCallEdge(edge)) {
			int frequency = model.getFrequency(edge);
//			if (frequency <= 1)
//				return 1;
//			if (frequency <= 3)
//				return 2;
//			if (frequency <= 5)
//				return 3;
//			if (frequency <= 10)
//				return 4;
//			return 5;
			if(frequency <= 8) {
				return frequency;
			}
			return 9; 
		}
		return 1;
	}
	
	public void adjustLineWidth(GraphModel model) {
		width = calculateLineWidth(model);
		LineType myLineType = LineType.createLineType(width, LineType.CAP_ROUND, LineType.JOIN_ROUND, (float) METER_LIMIT, null, DASH_PHASE);
		setLineType(myLineType);
	}
}


