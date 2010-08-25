package pdt.y.view.modes;

import y.base.YCursor;
import y.layout.Layouter;
import y.layout.router.OrthogonalEdgeRouter;
import y.view.Graph2D;
import y.view.MoveSelectionMode;

/*
* A special mode for moving a selection of the graph.
*/
public class MyMoveSelectionMode extends MoveSelectionMode {
	private static final boolean ROUTE_EDGES_ON_MOVE = true;
	private Layouter router = new OrthogonalEdgeRouter();
	
	
	public MyMoveSelectionMode(Layouter router) {
		super();
		this.router = router;
	}


	protected void selectionOnMove(double dx, double dy, double x, double y) {
		if (ROUTE_EDGES_ON_MOVE) {
			routeEdgesToSelection();
			super.selectionOnMove(dx, dy, x, y);
		}
	}

	protected void selectionMovedAction(double dx, double dy, double x, double y) {
		routeEdgesToSelection();
		super.selectionMovedAction(dx, dy, x, y);
	}

	void routeEdgesToSelection() {
		final Graph2D graph = view.getGraph2D();
		YCursor cursor= graph.selectedNodes();
		
		if (cursor.ok()) {
			router.doLayout(graph);
			graph.updateViews();
		}
	}
}
