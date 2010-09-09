package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.Graphics2D;

import pdt.y.model.GraphModel;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;
import y.view.SizeConstraintProvider;

public class MyShapeNodeRealizer extends ShapeNodeRealizer{

	public static final int INITIAL_STATE = 0;
	public static final int TRANSITION_STATE = 1;
	public static final int FINAL_STATE = 2;
	public static final byte CUSTOM_SHAPE = 4;
	protected final double MAX_NODE_HEIGHT = Double.MAX_VALUE;
	protected final double MAX_NODE_WIDTH = Double.MAX_VALUE;
	private int state;
	private GraphModel	 model;

	public MyShapeNodeRealizer(GraphModel  model){
		super(ShapeNodeRealizer.ROUND_RECT);
		this.state = TRANSITION_STATE;
		this.model= model;
		//this.setSize(40,40);
		this.setFillColor(Color.ORANGE);  
	}

	public MyShapeNodeRealizer(NodeRealizer r)
	{
		super(r);
		if(r instanceof MyShapeNodeRealizer)
		{
			MyShapeNodeRealizer sr = (MyShapeNodeRealizer)r;
			this.state = sr.state;
			this.model	= sr.model;
		}
		else
		{
			this.state = FINAL_STATE;
		}
	}
	@Override
	protected void paintNode(Graphics2D gfx) {

		if(model.getModule(this.getNode()).equals("transition")){
			this.setShapeType(TRIANGLE);
		}

		switch (state) {
		case INITIAL_STATE:
			gfx.setStroke(LineType.DASHED_1);
			gfx.setColor(Color.BLUE);
			break;
		case TRANSITION_STATE:
			this.setShapeType(ROUND_RECT);
			this.setFillColor(Color.WHITE);
			this.setLineColor(Color.BLUE);
			byte myStyle = LineType.LINE_2.getLineStyle();
			LineType myLineType = LineType.getLineType(2,myStyle);
			this.setLineType(myLineType);
			break;
		case FINAL_STATE: 
			gfx.setStroke(LineType.LINE_2);
			gfx.setColor(Color.GREEN);
			break;
		default:
			break;
		}
		super.paintNode(gfx);
	}

	public int getState() {
		return state;
	}

	public void setState(int initialState) {
		this.state  = initialState;

	}

	public SizeConstraintProvider getSizeConstraintProvider() {
		return new SizeConstraintProvider.Default(Math.max(1, getLabel().getWidth()), 
				Math.max(1, getLabel().getHeight()), MAX_NODE_WIDTH, MAX_NODE_HEIGHT);
	}

	protected void labelBoundsChanged(NodeLabel label) {
		if (label == getLabel()) {//only resize on bounds changes of the first label
			setSize(Math.max(30, label.getWidth()), Math.max(30, label.getHeight()));
		}
	}


	public NodeRealizer createCopy(NodeRealizer r)
	{
		return new MyShapeNodeRealizer(r);
	}

}
