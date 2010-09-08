package pdt.y.model.realizer;

import java.awt.Color;
import java.awt.geom.Rectangle2D;

import pdt.y.model.GraphModel;
import y.base.DataMap;
import y.base.Node;
import y.base.NodeCursor;
import y.geom.YDimension;
import y.geom.YInsets;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.SizeConstraintProvider;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;


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
    
 
    public void createModuleLabel() {
    	createHeaderLabel();
    	//createContentLabel();
    }

	private void createHeaderLabel() {
		NodeLabel label = getLabel();
    	label.setAlignment(NodeLabel.CENTER);
    	label.setBackgroundColor(Color.GREEN);
    	label.setTextColor(Color.BLACK);
    	label.setUnderlinedTextEnabled(true);
    	label.setModel(NodeLabel.INTERNAL);
	}
	
	public void createContentLabel() {
		Node node = getNode();
		HierarchyManager hierarchy = model.getHierarchyManager();
		NodeCursor nodeCursor = hierarchy.getChildren(node);
		DataMap allNodes = model.getNodeMap();
		while (nodeCursor.ok()) {
			Node childNode = nodeCursor.node();
			NodeLabel childLabel = new NodeLabel();
			childLabel.setText((String)allNodes.get(childNode));
			childLabel.bindRealizer(this);
		}
	}
	
	

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
