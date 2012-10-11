package pdt.y.focusview;

import pdt.y.focusview.FocusView;
import pdt.y.focusview.GraphPIFLoader;
import pdt.y.main.PDTGraphView;

public class GlobalView extends FocusView {
	
	@Override
	protected GraphPIFLoader createGraphPIFLoader(PDTGraphView pdtGraphView) {
		return new GlobalGraphPIFLoader(pdtGraphView, this);
	}
}
