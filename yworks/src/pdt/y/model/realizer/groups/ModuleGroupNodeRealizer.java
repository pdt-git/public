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
		label.setBackgroundColor(new Color(203, 215, 226));
		label.setTextColor(Color.BLACK);
		label.setUnderlinedTextEnabled(true);
		label.setModel(NodeLabel.INTERNAL);
		label.setConfiguration("CroppingLabel");
	}

	@Override
	public NodeRealizer createCopy(NodeRealizer nr) {
		return new ModuleGroupNodeRealizer(nr);
	}
}
