package pdt.y.view.swing.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import pdt.y.main.PDTGraphSwingStandalone;

public class ResetLayout extends AbstractAction{
	private static final long serialVersionUID = -5882701241202823622L;
	private PDTGraphSwingStandalone view;
	
	public ResetLayout(PDTGraphSwingStandalone pdtGraphSwing) {
		super("Reset Layout");
		this.view = pdtGraphSwing;
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		view.calcLayout();
		
		
	}

}
