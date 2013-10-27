package pdt.y.focusview;

import pdt.y.main.PDTGraphView;


public class GlobalView extends ViewBase {
	
	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		return new GlobalViewCoordinator(this);
	}
	
	@Override
	protected GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new GlobalGraphPIFLoader(pdtGraphView);
	}
}
