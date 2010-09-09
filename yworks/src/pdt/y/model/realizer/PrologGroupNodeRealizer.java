package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import pdt.y.model.GraphModel;
import y.base.Node;
import y.base.NodeCursor;
import y.geom.YDimension;
import y.geom.YInsets;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.SizeConstraintProvider;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

abstract public class PrologGroupNodeRealizer extends GroupNodeRealizer {

	private static double Y_OFFSET = 3.0;
	private static double X_OFFSET = 3.0;
	protected GraphModel model;
	private double totalLableHeight = 0.0;
	private double totalLableWidth = 0.0;

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
		setConsiderNodeLabelSize(true); 
		setAutoBoundsEnabled(true);
		YInsets minInsets = new YInsets(5,5,5,5);
		setMinimalInsets(minInsets);
		createHeaderLabel();
	}
    
    protected abstract void createHeaderLabel();
	
	
	public void paintText(Graphics2D gfx) {
		NodeLabel label = getLabel();
		label.paint(gfx);
		paintContentLabel(gfx);
		YDimension dimension = calculateMinSize();
		setSize(dimension.width, dimension.height);
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

	private void calculateLabelSize() {
		double height = Y_OFFSET;
		double width = 0;
		for (int a=0; a<labelCount(); a++){
			NodeLabel label= getLabel(a);
			height += label.getHeight()+Y_OFFSET;
			width = Math.max(width,label.getWidth());
		}
		NodeCursor nodeCursor = getNodeCursorForInnerNodes();
		if (nodeCursor != null) {
			while (nodeCursor.ok()) {
				Node childNode = nodeCursor.node();
				NodeLabel label = new NodeLabel();
				String labelText = model.getLabelTextForNode(childNode);
				label.setText(labelText);
				height += label.getHeight()+Y_OFFSET;
				width = Math.max(width,label.getWidth());
				nodeCursor.next();
			}
		}
		//width+= X_OFFSET *2;
		totalLableHeight = height;
		totalLableWidth = width;
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

	public SizeConstraintProvider getSizeConstraintProvider() {
		YDimension minSize = calculateMinSize();
		return new SizeConstraintProvider.Default(minSize, minSize);
	}

	private YDimension calculateMinSize() {
		calculateLabelSize();
		Rectangle2D minimalGroupBounds = calcMinimumGroupBounds();
		double innerGraphWidth = minimalGroupBounds.getWidth();
		double maxWidth = Math.max(innerGraphWidth, totalLableWidth);
		
		double innerGraphHeight = minimalGroupBounds.getHeight();
		double maxHeight = innerGraphHeight + totalLableHeight;
		
		return new YDimension(maxWidth, maxHeight);
	}

}