package pdt.y.model.realizer.edges;

import java.awt.Color;

import pdt.y.model.GraphModel;
import y.base.Edge;
import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.LineType;

public class CallEdgeRealizer extends GenericEdgeRealizer {

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
		setLineColor(Color.DARK_GRAY);
		width = (float)1.0;
		LineType myLineType = LineType.createLineType(width, LineType.CAP_ROUND, LineType.JOIN_ROUND, (float) METER_LIMIT, null, DASH_PHASE);
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
