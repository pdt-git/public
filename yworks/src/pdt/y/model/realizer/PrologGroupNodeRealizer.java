package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.Graphics2D;
import pdt.y.model.GraphModel;
import y.base.Node;
import y.base.NodeCursor;
import y.geom.YInsets;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

abstract public class PrologGroupNodeRealizer extends GroupNodeRealizer {

	private static double Y_OFFSET = 3.0;
	private static double X_OFFSET = 3.0;
	protected GraphModel model;
	private boolean showContentAsLabel = false;

	public PrologGroupNodeRealizer(GraphModel model) {
		super();
		this.model=model;
		init();
	}

	public PrologGroupNodeRealizer(NodeRealizer nr) {
		super(nr);
		if(nr instanceof PrologGroupNodeRealizer)
		{
			PrologGroupNodeRealizer sr = (PrologGroupNodeRealizer)nr;
			this.model	= sr.model;
		}
		init();
	}

	protected void init() {
		setFillColor(Color.YELLOW);
		setShapeType(GroupNodeRealizer.ROUND_RECT);
		//setConsiderNodeLabelSize(true); 
		setAutoBoundsEnabled(true);
		YInsets minInsets = new YInsets(5,5,5,5);
		setMinimalInsets(minInsets);
		createHeaderLabel();
	}
    
    protected abstract void createHeaderLabel();
	
	@Override
	public void paintText(Graphics2D gfx) {
		NodeLabel label = getLabel();
		label.paint(gfx);
		if (showContentAsLabel) {
			paintContentLabel(gfx);
		}
//		YDimension dimension = calculateMinSize();
//		setSize(dimension.width, dimension.height);
	}

	private NodeLabel paintAnInnerLabel(Graphics2D gfx, String labelText, double yOffset) {
		NodeLabel childLabel = new NodeLabel();
		childLabel.setText(labelText);
		childLabel.setModel(NodeLabel.FREE);
		childLabel.setOffset(X_OFFSET, yOffset);
		childLabel.bindRealizer(this);
		childLabel.paint(gfx);
		return childLabel;
	}



	private void paintContentLabel(Graphics2D gfx) {
		NodeCursor nodeCursor = getNodeCursorForInnerNodes();
	
		double momentaryLabelHeight= getLabel().getHeight()+ Y_OFFSET;
		while (nodeCursor.ok()) {
			Node childNode = nodeCursor.node();
			String labelText = model.getLabelTextForNode(childNode);
			NodeLabel childLabel = paintAnInnerLabel(gfx, labelText, momentaryLabelHeight);
			//gfx.setColor(getLineColor());
			//gfx.drawLine((int)x+1,(int)(y+actualYOffset),(int)(x+width-1),(int)(y+labelHeight));
			momentaryLabelHeight += childLabel.getHeight() + Y_OFFSET;
		
			nodeCursor.next();
		}
	}

	private NodeCursor getNodeCursorForInnerNodes() {
		Node node = getNode();
		HierarchyManager hierarchy = model.getHierarchyManager();
		NodeCursor nodeCursor = null;
		try{
			nodeCursor = hierarchy.getChildren(node);
		} catch (NullPointerException e) {
		}
		return nodeCursor;
	}

/*	@Override
	public SizeConstraintProvider getSizeConstraintProvider() {
		YDimension minSize = calculateMinSize();
		return new SizeConstraintProvider.Default(minSize, minSize);
	}

	private YDimension calculateMinSize() {
		if (showContentAsLabel) {
			calculateLabelSize();
		}
		Rectangle2D minimalGroupBounds = calcMinimumGroupBounds();
		double innerGraphWidth = minimalGroupBounds.getWidth();
		double maxWidth = Math.max(innerGraphWidth, totalLableWidth)+50.0;
		
		double innerGraphHeight = minimalGroupBounds.getHeight();
		double maxHeight = innerGraphHeight + totalLableHeight+50.0;
		
		return new YDimension(maxWidth, maxHeight);
	}*/

}