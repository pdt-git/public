package pdt.y.model.realizer.groups;

import java.awt.Color;

import pdt.y.model.GraphModel;
import y.view.NodeLabel;
import y.view.NodeRealizer;


public class ModuleGroupNodeRealizer extends PrologGroupNodeRealizer {

	public ModuleGroupNodeRealizer(GraphModel model) {
		super(model);
	}

	public ModuleGroupNodeRealizer(NodeRealizer nr) {
		super(nr);
	}

	@Override
	protected void createHeaderLabel() {
		NodeLabel label = getLabel();
		label.setAlignment(NodeLabel.CENTER);
		label.setBackgroundColor(Color.ORANGE);
		label.setTextColor(Color.BLACK);
		label.setUnderlinedTextEnabled(true);
		label.setModel(NodeLabel.INTERNAL);
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer nr) {
		return new ModuleGroupNodeRealizer(nr);
	}
}
