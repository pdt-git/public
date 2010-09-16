package pdt.y.model.realizer;

import java.awt.Color;

import pdt.y.model.GraphModel;

import y.base.DataMap;
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
	
	private int calculateLineWidth() {
		Edge edge = getEdge();
		GraphModel model = GraphModel.getInstance();
		if(model.isCallEdge(edge)) {
			DataMap frequencyMap = model.getCallFrequencyMap();
			int frequency = frequencyMap.getInt(edge);
			if (frequency <= 1)
				return 1;
			if (frequency <= 3)
				return 2;
			if (frequency <= 5)
				return 3;
			if (frequency <= 10)
				return 4;
			return 5;
		}
		return 1;
	}
	
	public void adjustLineWidth() {
		width = calculateLineWidth();
		LineType myLineType = LineType.createLineType(width, LineType.CAP_ROUND, LineType.JOIN_ROUND, (float) METER_LIMIT, null, DASH_PHASE);
		setLineType(myLineType);
	}
}
