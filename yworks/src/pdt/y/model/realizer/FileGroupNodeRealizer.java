package pdt.y.model.realizer;

import java.awt.Color;

import pdt.y.model.GraphModel;
import y.view.NodeLabel;
import y.view.NodeRealizer;


public class FileGroupNodeRealizer extends PrologGroupNodeRealizer {

	public FileGroupNodeRealizer(GraphModel model) {
		super(model);
	}

	public FileGroupNodeRealizer(NodeRealizer nr) {
		super(nr);
	}

	@Override
	protected void createHeaderLabel() {
		NodeLabel label = getLabel();
		label.setAlignment(NodeLabel.LEFT);
		label.setBackgroundColor(Color.ORANGE);
		label.setTextColor(Color.BLUE);
		label.setUnderlinedTextEnabled(true);
		label.setModel(NodeLabel.INTERNAL);
	}

	public NodeRealizer createCopy(NodeRealizer nr) {
		return new FileGroupNodeRealizer(nr);
	}
}
