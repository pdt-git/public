package pdt.y.model.realizer;

import java.awt.Color;

import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.LineType;

public class CallEdgeRealizer extends GenericEdgeRealizer {

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
		byte myStyle = LineType.LINE_1.getLineStyle();
		LineType myLineType = LineType.getLineType(1,myStyle);
		setLineType(myLineType);
	}
}
