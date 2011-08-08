package pdt.y.model.realizer.nodes;

import java.awt.Color;
import java.awt.Graphics2D;

import pdt.y.model.GraphModel;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;
import y.view.SizeConstraintProvider;


public class PredicateNodeRealizer extends ShapeNodeRealizer{

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
		//this.setSize(40,40);
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
	}
	@Override
	protected void paintNode(Graphics2D gfx) {
		//		switch (state) {
		//		case INITIAL_STATE:
		//			gfx.setStroke(LineType.DASHED_1);
		//			gfx.setColor(Color.BLUE);
		//			break;
		//		case TRANSITION_STATE:

		byte myStyle;
		if (model.getDataHolder().isDynamicNode(getNode())) {
			myStyle = LineType.DASHED_DOTTED_2.getLineStyle();
		} else {
			myStyle = LineType.LINE_2.getLineStyle();
		}
		LineType myLineType = LineType.getLineType(2,myStyle);
		setLineType(myLineType);

		if (model.getDataHolder().isMetaPred(getNode())) {
			setShapeType(HEXAGON);
		} else if (model.getDataHolder().isTransparentNode(getNode())) {
			setShapeType(ELLIPSE);
		} else {
			setShapeType(ROUND_RECT);
		}

		if (model.getDataHolder().isExported(getNode())) {
			setFillColor(Color.GREEN);
		} else {
			setFillColor(Color.YELLOW);
		}

		if (model.getDataHolder().isUnusedLocal(getNode())) {
			setLineColor(Color.RED);
		} else {
			setLineColor(Color.BLACK);
		}

		//			break;
		//		case FINAL_STATE:
		//			gfx.setStroke(LineType.LINE_2);
		//			gfx.setColor(Color.GREEN);
		//			break;
		//		default:
		//			break;
		//		}
		super.paintNode(gfx);
	}

	public int getState() {
		return state;
	}

	public void setState(int initialState) {
		state  = initialState;

	}

	@Override
	public SizeConstraintProvider getSizeConstraintProvider() {
		return new SizeConstraintProvider.Default(Math.max(1, getLabel().getWidth()),
				Math.max(1, getLabel().getHeight()), MAX_NODE_WIDTH, MAX_NODE_HEIGHT);
	}

	@Override
	protected void labelBoundsChanged(NodeLabel label) {
		if (label == getLabel()) {//only resize on bounds changes of the first label
			setSize(Math.max(30, label.getWidth()), Math.max(30, label.getHeight()));
		}
	}


	@Override
	public NodeRealizer createCopy(NodeRealizer r)
	{
		return new PredicateNodeRealizer(r);
	}

}
