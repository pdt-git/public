package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.geom.Rectangle2D;

import pdt.y.model.GraphModel;
import y.geom.YDimension;
import y.geom.YInsets;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.SizeConstraintProvider;
import y.view.hierarchy.GroupNodeRealizer;


  public class MyGroupNodeRealizer extends GroupNodeRealizer {
	  private GraphModel  model;
    public MyGroupNodeRealizer(GraphModel model) {
      super();
      this.model=model;
      init();
    }

	public MyGroupNodeRealizer(NodeRealizer nr) {
      super(nr);
		if(nr instanceof MyGroupNodeRealizer)
		{
			MyGroupNodeRealizer sr = (MyGroupNodeRealizer)nr;
			this.model	= sr.model;
		}
      init();
    }

    private void init() {
		setFillColor(Color.YELLOW);
		setShapeType(GroupNodeRealizer.ROUND_RECT);
		setConsiderNodeLabelSize(true); 
		setAutoBoundsEnabled(true);
		YInsets minInsets = new YInsets(5,5,5,5);
		setMinimalInsets(minInsets);
		createFileLabel();
	}
    
    private void createFileLabel() {
		createHeaderLabel();
    }
    
    private void createHeaderLabel() {
    	NodeLabel label = getLabel();
    	label.setAlignment(NodeLabel.LEFT);
    	label.setBackgroundColor(Color.ORANGE);
    	label.setTextColor(Color.BLUE);
    	label.setUnderlinedTextEnabled(true);
    	label.setModel(NodeLabel.INTERNAL);
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
