package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.geom.Rectangle2D;

import pdt.y.model.GraphModel;

import y.base.DataMap;
import y.base.Node;
import y.base.YList;
import y.geom.YDimension;
import y.geom.YInsets;
import y.geom.YPoint;
import y.geom.YRectangle;
import y.layout.NodeLabelLayout;
import y.layout.NodeLabelModel;
import y.layout.NodeLayout;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;
import y.view.SizeConstraintProvider;
import y.view.hierarchy.GroupNodeRealizer;


  public class ModuleGroupNodeRealizer extends GroupNodeRealizer {
	  private GraphModel  model;
    public ModuleGroupNodeRealizer(GraphModel model) {
      super();
      this.model=model;
      init();
    }

	public ModuleGroupNodeRealizer(NodeRealizer nr) {
      super(nr);
		if(nr instanceof ModuleGroupNodeRealizer)
		{
			ModuleGroupNodeRealizer sr = (ModuleGroupNodeRealizer)nr;
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
		createModuleLabel();
	}
    
    
    
//    private void initializeDesignOfHeaderLabel() {
//		DataMap moduleMap = model.getModuleMap();
//		Node node = getNode();
//    	NodeLabel label = getLabel(); //this gets the first label only
//		String labelText = (String)moduleMap.get(node);
//		createFileLabel(label);
//    }
//    
//    private void createFileLabel(NodeLabel label) {
//    	label.setAlignment(NodeLabel.LEFT);
//    	label.setBackgroundColor(Color.ORANGE);
//    	label.setTextColor(Color.BLUE);
//    	label.setUnderlinedTextEnabled(true);
//    	label.setModel(NodeLabel.INTERNAL);
//    }
 
    public void createModuleLabel() {
    //	for (int i=1; i<=labelCount(); i++) {
    //		NodeLabel label = getLabel(i);
    	NodeLabel label = getLabel();
    		
    		label.setAlignment(NodeLabel.CENTER);
    		label.setBackgroundColor(Color.GREEN);
    		label.setTextColor(Color.BLACK);
    		label.setUnderlinedTextEnabled(true);
    		label.setModel(NodeLabel.INTERNAL);
    //	}
    }
    
    
//    @Override
//	public void paintText(Graphics2D gfx) {
//    	NodeLabel label = new NodeLabel();
//    	label.setAlignment(NodeLabel.CENTER);
//		label.setBackgroundColor(Color.GREEN);
//		label.setTextColor(Color.BLACK);
//		label.setUnderlinedTextEnabled(true);
//		label.setModel(NodeLabel.INTERNAL);
//		label.bindRealizer(this);
//		label.paint(gfx);
//    }

	public NodeRealizer createCopy(NodeRealizer nr) {
      return new ModuleGroupNodeRealizer(nr);
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
