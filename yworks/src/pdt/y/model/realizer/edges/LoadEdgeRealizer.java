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

import java.awt.Color;

import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.LineType;

public class LoadEdgeRealizer extends GenericEdgeRealizer {

	public LoadEdgeRealizer() {
		super();
		init();
	}
	
	public LoadEdgeRealizer(EdgeRealizer realizer){
		super(realizer);
		init();
	}
	
	private void init() {
		setSourceArrow(Arrow.DELTA);
		setLineColor(Color.BLUE);
		byte myStyle = LineType.LINE_3.getLineStyle();
		LineType myLineType = LineType.getLineType(4,myStyle);
		setLineType(myLineType);
	}
}


