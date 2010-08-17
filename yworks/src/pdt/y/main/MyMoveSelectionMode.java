package pdt.y.main;

import y.layout.Layouter;
import y.view.Graph2D;
import y.view.MoveSelectionMode;

/*
* A special mode for moving a selection of the graph.
*/
class MyMoveSelectionMode extends MoveSelectionMode {
	private static final boolean ROUTE_EDGES_ON_MOVE = true;
	private Layouter router;
	
	
	public MyMoveSelectionMode(Layouter router) {
		super();
		this.router = router;
	}


	protected void selectionOnMove(double dx, double dy, double x, double y) {
		if (ROUTE_EDGES_ON_MOVE) {
			routeEdgesToSelection();
		}
	}

	protected void selectionMovedAction(double dx, double dy, double x, double y) {
		routeEdgesToSelection();
	}

	void routeEdgesToSelection() {
		final Graph2D graph = view.getGraph2D();
		
		if (graph.selectedNodes().ok()) {
			router.doLayout(graph);
			graph.updateViews();
		}
	}
}
