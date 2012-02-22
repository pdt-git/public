package pdt.y.model;

import pdt.y.preferences.PredicateLayoutPreferences;
import pdt.y.preferences.PreferenceConstants;
import y.layout.CompositeLayoutStage;
import y.layout.LayoutStage;
import y.layout.Layouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.organic.SmartOrganicLayouter;
import y.layout.router.OrthogonalEdgeRouter;

public class GraphLayout {

	private Layouter hierarhicLayouter;
	private Layouter organicLayouter;
	private LayoutStage edgeLayouter; 

	
	public GraphLayout() {
		
		edgeLayouter = createEdgeLayout();
		hierarhicLayouter = createHierarchicalLayouter();
		organicLayouter = createOrganicLayouter();
	}


	protected LayoutStage createEdgeLayout() {
		OrthogonalEdgeRouter router = new OrthogonalEdgeRouter();
	    router.setLocalCrossingMinimizationEnabled(true);
	    router.setReroutingEnabled(true);
		return router;
	}


	protected Layouter createHierarchicalLayouter() {
		IncrementalHierarchicLayouter layouter = new IncrementalHierarchicLayouter();
		
		//set some options
//		layouter.getNodeLayoutDescriptor().setMinimumLayerHeight(2);
//		layouter.getNodeLayoutDescriptor().setMinimumDistance(10);

		
		//use top-to-bottom layout orientation
		// the  layouter.setOrientation(..) is not working therefore set orientation manually
//		OrientationLayouter ol = new OrientationLayouter();
//		ol.setOrientation(LayoutOrientation.TOP_TO_BOTTOM);
//		layouter.setOrientationLayouter(ol);
//		
//		layouter.setFromScratchLayeringStrategy(IncrementalHierarchicLayouter.LAYERING_STRATEGY_HIERARCHICAL_TIGHT_TREE);
//		layouter.setGroupAlignmentPolicy(IncrementalHierarchicLayouter.POLICY_ALIGN_GROUPS_CENTER);

		
//	    final AdaptNodeToLabelWidths stage = new AdaptNodeToLabelWidths();
//	    stage.setAdaptGroupNodesOnly(false);

//	    final HierarchicLayouter hl = layouter.getHierarchicLayouter();
//	    final DrawingDistanceCalculator ddc = hl.getDrawingDistanceCalculator();
	    // drawing distance calculator that calculates a minimum width for all
	    // non-empty group nodes in such a way that node.x is less than or equal to
	    // the minimum x-coordinate of all node labels and node.x + node.width is
	    // greater than or equal to the maximum x-coordinate of all node labels
	    //
	    // this is done due to the fact that non-empty group nodes are an exception
	    // to the general "no node resizing" policy of yFiles layout algorithms
//	    final LabelAwareDrawingDistanceCalculator laddc =
//	            new LabelAwareDrawingDistanceCalculator(ddc);
//	    laddc.setGroupLabelWidthAdjustment(2.0);
//	    hl.setDrawingDistanceCalculator(laddc);

	    // horizontal group compaction tries to prevent
	    // IncrementalHierarchicLayouter from being to generous when calculating
	    // group node sizes for non-empty group nodes
//	    SimplexNodePlacer snp = new SimplexNodePlacer();
//	    snp.setGroupCompactionStrategy(SimplexNodePlacer.GROUP_COMPACTION_MAX);
//		snp.setEdgeStraighteningOptimizationEnabled(true);
//		layouter.setGroupCompactionEnabled(true);
//		layouter.setRecursiveGroupLayeringEnabled(true);
//	    layouter.setNodePlacer(snp);
	   
	    
	    
	    // Crashes when doing  doLayout manually
//	    layouter.setAutomaticEdgeGroupingEnabled(true);
	    
		return layouter;
	}
	
	private Layouter createOrganicLayouter() {
		SmartOrganicLayouter layouter = new SmartOrganicLayouter();
		layouter.setMinimalNodeDistance(20);
		layouter.setNodeEdgeOverlapAvoided(true);
		return layouter;
	}
	
	public Layouter getLayouter(){
		CompositeLayoutStage stage  = new CompositeLayoutStage();
		
		if (PredicateLayoutPreferences.getLayoutPreference().equals(PreferenceConstants.LAYOUT_HIERARCHY)) {
			
			stage.setCoreLayouter(hierarhicLayouter);
		}
		else {
			stage.setCoreLayouter(organicLayouter);
		}
		
		stage.appendStage(edgeLayouter);
		
		return stage;
	}
	
	public Layouter getEdgeLayouter(){
		return this.edgeLayouter;
	}
	
	
}
