package pdt.y.model.realizer.nodes;

import pdt.y.utils.Size;
import y.view.NodeLabel;
import y.view.NodeRealizer;
import y.view.ShapeNodeRealizer;

public class NodeRealizerBase extends ShapeNodeRealizer {

	public NodeRealizerBase() {
		super();
	}
	
	public NodeRealizerBase(NodeRealizer r) {
		super(r);
	}

	public NodeRealizerBase(byte roundRect) {
		super(roundRect);
	}

	public String getInfoText() {
		return "";
	}

	public Size calcLabelSize(String text) {
		NodeLabel l = new NodeLabel();
		l.setText(text);
		l.setModel(NodeLabel.FREE);
		l.bindRealizer(this);
		return new Size((int)l.getWidth(), (int)l.getHeight());
	}
}
