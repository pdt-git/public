package pdt.y.model.realizer.edges;

import pdt.y.model.GraphModel;
import pdt.y.utils.LogtalkStyles;
import y.base.Edge;
import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;

public class LogtalkEdgeRealizer extends GenericEdgeRealizer implements InfoTextProvider {

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
		Arrow sourceArrow = logtalkStyles.getSourceArrow();
		if (sourceArrow != null) {
			setSourceArrow(sourceArrow);
		}
	}
	
	@Override
	public String getInfoText() {
		return getLabel().getText();
	}

}
