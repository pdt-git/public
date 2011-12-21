package pdt.y.view.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import pdt.y.main.PDTGraphView;

public class ResetLayout extends AbstractAction{
	private static final long serialVersionUID = -5882701241202823622L;
	private PDTGraphView view;
	
	public ResetLayout(PDTGraphView pdtGraphSwing) {
		super("Reset Layout");
		this.view = pdtGraphSwing;
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		view.calcLayout();
		
		
	}

}
