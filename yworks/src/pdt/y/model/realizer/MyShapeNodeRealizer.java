package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.Graphics2D;

import pdt.y.model.GraphModel;

import y.view.LineType;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;

public class MyShapeNodeRealizer extends ShapeNodeRealizer{
	
	public static final int INITIAL_STATE = 0;
	public static final int TRANSITION_STATE = 1;
	public static final int FINAL_STATE = 2;
	public static final byte CUSTOM_SHAPE = 4;
	private int state;
	private GraphModel	 model;

	public MyShapeNodeRealizer(GraphModel  model){
		super(ShapeNodeRealizer.ROUND_RECT);
		this.state = TRANSITION_STATE;
		this.model= model;
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
			this.setShapeType(TRAPEZOID);
			this.setFillColor(Color.MAGENTA);
			this.setFillColor2(Color.BLUE);
			this.setLineColor(Color.GREEN);
			byte myStyle = LineType.DASHED_DOTTED_1.getLineStyle();
			LineType myLineType = LineType.getLineType(3,myStyle);
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

	  public NodeRealizer createCopy(NodeRealizer r)
	  {
	    return new MyShapeNodeRealizer(r);
	  }

}
