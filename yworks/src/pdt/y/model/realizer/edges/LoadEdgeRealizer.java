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
