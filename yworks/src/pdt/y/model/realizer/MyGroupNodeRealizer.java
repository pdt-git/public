package pdt.y.model.realizer;

import java.awt.Color;

import pdt.y.model.GraphModel;
import y.view.NodeLabel;
import y.view.NodeRealizer;


public class MyGroupNodeRealizer extends PrologGroupNodeRealizer {

	public MyGroupNodeRealizer(GraphModel model) {
		super(model);
	}

	public MyGroupNodeRealizer(NodeRealizer nr) {
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
		return new MyGroupNodeRealizer(nr);
	}
}
