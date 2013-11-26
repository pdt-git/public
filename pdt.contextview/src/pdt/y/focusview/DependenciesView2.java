package pdt.y.focusview;

import pdt.y.main.PDTGraphView;


public class DependenciesView2 extends GlobalView {
	
	@Override
	public GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new DependenciesGraphPIFLoader2(pdtGraphView);
	}
}
