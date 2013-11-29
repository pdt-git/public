package pdt.y.focusview;

import pdt.y.main.PDTGraphView;


public class LogtalkEntityView extends GlobalView {
	
	@Override
	public GraphPIFLoaderBase createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new LogtalkEntityGraphPIFLoader(pdtGraphView);
	}
}
