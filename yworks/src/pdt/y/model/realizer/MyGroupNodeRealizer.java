package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.geom.Rectangle2D;

import y.geom.YDimension;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;
import y.view.SizeConstraintProvider;
import y.view.hierarchy.GroupNodeRealizer;


  public class MyGroupNodeRealizer extends GroupNodeRealizer {
    protected final double MAX_NODE_HEIGHT = Double.MAX_VALUE;
    protected final double MAX_NODE_WIDTH = Double.MAX_VALUE;

    public MyGroupNodeRealizer() {
      super();
      init();
    }

	public MyGroupNodeRealizer(NodeRealizer nr) {
      super(nr);
      init();
    }

    private void init() {
		setFillColor(Color.YELLOW);
		setShapeType(GroupNodeRealizer.ROUND_RECT);
		setConsiderNodeLabelSize(true); 
		setAutoBoundsEnabled(true);
	}

	public NodeRealizer createCopy(NodeRealizer nr) {
      return new MyGroupNodeRealizer(nr);
    }

	public SizeConstraintProvider getSizeConstraintProvider() {
		YDimension minSize = calculateMinSize();
		return new SizeConstraintProvider.Default(minSize, minSize);
	}

	private YDimension calculateMinSize() {
		Rectangle2D minimalGroupBounds = calcMinimumGroupBounds();
		double maxWidth = minimalGroupBounds.getWidth();
		maxWidth = Math.max(maxWidth, getLabel().getWidth());
		double maxHeight = minimalGroupBounds.getHeight();
		return new YDimension(maxWidth, maxHeight);
	}
  }
