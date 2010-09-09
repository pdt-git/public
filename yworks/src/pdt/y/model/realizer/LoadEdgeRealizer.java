package pdt.y.model.realizer;

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
		setTargetArrow(Arrow.DELTA);
		setLineColor(Color.BLUE);
		byte myStyle = LineType.LINE_3.getLineStyle();
		LineType myLineType = LineType.getLineType(2,myStyle);
		setLineType(myLineType);
	}
}
