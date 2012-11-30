package pdt.y.focusview;

import pdt.y.main.PDTGraphView;


public class FocusView extends ViewBase {
	
	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		return new FocusViewCoordinator(this);
	}
	
	@Override
	public GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new FocusGraphPIFLoader(pdtGraphView);
	}
}
