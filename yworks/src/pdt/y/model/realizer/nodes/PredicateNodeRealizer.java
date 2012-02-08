package pdt.y.model.realizer.nodes;

import java.awt.Color;
import java.awt.Graphics2D;

import pdt.y.model.GraphModel;
import pdt.y.preferences.AppearancePreferences;
import pdt.y.preferences.LayoutPreferences;
import y.geom.YDimension;
import y.view.LineType;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;


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
		
		init();
	}

	protected void init() {
		
		NodeLabel label = getLabel();
		
		label.setConfiguration(LayoutPreferences.getNameCroppingConfiguration());
		label.setAutoSizePolicy(NodeLabel.AUTOSIZE_NODE_SIZE);
		
		label.setUserData(new YDimension(getWidth(), getHeight()));
	}
	
	@Override
	protected void paintNode(Graphics2D gfx) {
		
		byte myStyle;
		if (model.getDataHolder().isDynamicNode(getNode())) {
			myStyle = AppearancePreferences.getDynamicPredicateBorderStyle().getLineStyle();
		} else {
			myStyle = AppearancePreferences.getBorderStyle().getLineStyle();
		}
		LineType myLineType = LineType.getLineType((int)AppearancePreferences.getBorderWidth(), myStyle);
		setLineType(myLineType);

		if (model.getDataHolder().isMetaPred(getNode())) {
			setShapeType(HEXAGON);
		} else if (model.getDataHolder().isTransparentNode(getNode())) {
			setShapeType(ELLIPSE);
		} else {
			setShapeType(ROUND_RECT);
		}

		if (model.getDataHolder().isExported(getNode())) {
			setFillColor(AppearancePreferences.getExportedPredicateColor());
		} else {
			setFillColor(AppearancePreferences.getPredicateColor());
		}

		if (model.getDataHolder().isUnusedLocal(getNode())) {
			setLineColor(AppearancePreferences.getUnusedPredicateBorderColor());
		} else {
			setLineColor(AppearancePreferences.getBorderColor());
		}
		
		super.paintNode(gfx);
	}

	public int getState() {
		return state;
	}

	public void setState(int initialState) {
		state  = initialState;
	}

	@Override
	protected void labelBoundsChanged(NodeLabel arg0) {
		getLabel().setUserData(new YDimension(getWidth(), getHeight()));
		
		super.labelBoundsChanged(arg0);
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer r)
	{
		return new PredicateNodeRealizer(r);
	}

}
