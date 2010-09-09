package pdt.y.model.realizer;

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
    	label.setBackgroundColor(Color.GREEN);
    	label.setTextColor(Color.BLACK);
    	label.setUnderlinedTextEnabled(true);
    	label.setModel(NodeLabel.INTERNAL);
	}

	public NodeRealizer createCopy(NodeRealizer nr) {
	  return new ModuleGroupNodeRealizer(nr);
	}
  }
