package pdt.y.model;

import y.layout.CompositeLayoutStage;
import y.layout.LayoutOrientation;
import y.layout.Layouter;
import y.layout.OrientationLayouter;
import y.layout.hierarchic.IncrementalHierarchicLayouter;
import y.layout.router.OrthogonalEdgeRouter;

public class GraphLayout {

	private CompositeLayoutStage stage;
	private Layouter coreLayouter;
	private OrthogonalEdgeRouter edgeLayouter; 

	
	public GraphLayout() {
		stage  = new CompositeLayoutStage();
		edgeLayouter = createEdgeLayout();
		coreLayouter = createCoreLayout();
		
		stage.setCoreLayouter(coreLayouter);
		stage.appendStage(edgeLayouter);
	}


	protected OrthogonalEdgeRouter createEdgeLayout() {
		OrthogonalEdgeRouter router = new OrthogonalEdgeRouter();
		return router;
	}


	protected Layouter createCoreLayout() {
		IncrementalHierarchicLayouter layout = new IncrementalHierarchicLayouter();
		
		//set some options
		layout.getNodeLayoutDescriptor().setMinimumLayerHeight(60);
		layout.getNodeLayoutDescriptor().setMinimumDistance(20);

		//use left-to-right layout orientation
		OrientationLayouter ol = new OrientationLayouter();
		ol.setOrientation(LayoutOrientation.BOTTOM_TO_TOP);
		layout.setOrientationLayouter(ol);
		layout.setBackloopRoutingEnabled(true);
		layout.setFromScratchLayeringStrategy(IncrementalHierarchicLayouter.LAYERING_STRATEGY_HIERARCHICAL_TOPMOST);
	
		return layout;
	}
	
	public Layouter getLayouter(){
		return this.stage;
	}
	
	public Layouter getEdgeLayouter(){
		return this.edgeLayouter;
	}
	
	
}
