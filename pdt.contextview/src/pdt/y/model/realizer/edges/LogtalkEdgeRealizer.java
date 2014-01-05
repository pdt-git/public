package pdt.y.model.realizer.edges;

import pdt.y.model.GraphModel;
import pdt.y.utils.LogtalkStyles;
import y.base.Edge;
import y.view.EdgeRealizer;

public class LogtalkEdgeRealizer extends EdgeRealizerBase {

	public LogtalkEdgeRealizer() {
	}

	public LogtalkEdgeRealizer(EdgeRealizer realizer) {
		super(realizer);
	}

	public void init(GraphModel graphModel) {
		Edge edge = getEdge();
		setLabelText(graphModel.getDataHolder().getEdgeLabel(edge));
		String edgeStyle = graphModel.getDataHolder().getEdgeStyle(edge);
		LogtalkStyles logtalkStyles = new LogtalkStyles(edgeStyle);
		setSourceArrow(logtalkStyles.getTargetArrow());
	}
	
	@Override
	public String getInfoText() {
		return getLabel().getText();
	}

}
