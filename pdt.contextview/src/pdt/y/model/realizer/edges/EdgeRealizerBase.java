package pdt.y.model.realizer.edges;

import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;

public class EdgeRealizerBase extends GenericEdgeRealizer {

	public EdgeRealizerBase() {
	}
	
	public EdgeRealizerBase(EdgeRealizer realizer) {
		super(realizer);
	}

	public String getInfoText() {
		return "";
	}
}
