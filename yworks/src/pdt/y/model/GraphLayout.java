package pdt.y.model;

import y.layout.CompositeLayoutStage;
import y.layout.LayoutOrientation;
import y.layout.LayoutStage;
import y.layout.Layouter;
import y.layout.OrientationLayouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.hierarchic.incremental.DrawingDistanceCalculator;
import y.layout.hierarchic.incremental.HierarchicLayouter;
import y.layout.hierarchic.incremental.SimplexNodePlacer;
import y.layout.router.OrthogonalEdgeRouter;

public class GraphLayout {

	private CompositeLayoutStage stage;
	private Layouter coreLayouter;
	private LayoutStage edgeLayouter; 

	
	public GraphLayout() {
		stage  = new CompositeLayoutStage();
		edgeLayouter = createEdgeLayout();
		coreLayouter = createCoreLayout();
		
		stage.setCoreLayouter(coreLayouter);
		stage.appendStage(edgeLayouter);
	}


	protected LayoutStage createEdgeLayout() {
		OrthogonalEdgeRouter router = new OrthogonalEdgeRouter();
	    router.setLocalCrossingMinimizationEnabled(true);
	    router.setCrossingCost(2.0);
	    router.setReroutingEnabled(true);
		return router;
	}


	protected Layouter createCoreLayout() {
		IncrementalHierarchicLayouter layouter = new IncrementalHierarchicLayouter();
		
		//set some options
		layouter.getNodeLayoutDescriptor().setMinimumLayerHeight(2);
		layouter.getNodeLayoutDescriptor().setMinimumDistance(10);

		
		//use top-to-bottom layout orientation
		// the  layouter.setOrientation(..) is not working therefore set orientation manually
		OrientationLayouter ol = new OrientationLayouter();
		ol.setOrientation(LayoutOrientation.TOP_TO_BOTTOM);
		layouter.setOrientationLayouter(ol);
		
		
		layouter.setFromScratchLayeringStrategy(IncrementalHierarchicLayouter.LAYERING_STRATEGY_HIERARCHICAL_TIGHT_TREE);
		layouter.setGroupAlignmentPolicy(IncrementalHierarchicLayouter.POLICY_ALIGN_GROUPS_CENTER);

		
	    final AdaptNodeToLabelWidths stage = new AdaptNodeToLabelWidths();
	    stage.setAdaptGroupNodesOnly(false);


	    layouter.prependStage(stage);
	    final HierarchicLayouter hl = layouter.getHierarchicLayouter();
	    final DrawingDistanceCalculator ddc = hl.getDrawingDistanceCalculator();
	    // drawing distance calculator that calculates a minimum width for all
	    // non-empty group nodes in such a way that node.x is less than or equal to
	    // the minimum x-coordinate of all node labels and node.x + node.width is
	    // greater than or equal to the maximum x-coordinate of all node labels
	    //
	    // this is done due to the fact that non-empty group nodes are an exception
	    // to the general "no node resizing" policy of yFiles layout algorithms
	    final LabelAwareDrawingDistanceCalculator laddc =
	            new LabelAwareDrawingDistanceCalculator(ddc);
	    laddc.setGroupLabelWidthAdjustment(2.0);
	    hl.setDrawingDistanceCalculator(laddc);

	    // horizontal group compaction tries to prevent
	    // IncrementalHierarchicLayouter from being to generous when calculating
	    // group node sizes for non-empty group nodes
	    SimplexNodePlacer snp = new SimplexNodePlacer();
	    snp.setGroupCompactionStrategy(SimplexNodePlacer.GROUP_COMPACTION_MAX);
		layouter.setGroupCompactionEnabled(true);
		layouter.setRecursiveGroupLayeringEnabled(true);
	    layouter.setNodePlacer(snp);
	   
	    
	    
	    // Crashes when doing  doLayout manually
	    layouter.setAutomaticEdgeGroupingEnabled(true);
	    
		return layouter;
	}
	
	public Layouter getLayouter(){
		return this.stage;
	}
	
	public Layouter getEdgeLayouter(){
		return this.edgeLayouter;
	}
	
	
}
