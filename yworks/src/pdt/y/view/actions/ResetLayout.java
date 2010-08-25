package pdt.y.view.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

import pdt.y.main.GraphPDTDemo;

public class ResetLayout extends AbstractAction{
	private static final long serialVersionUID = -5882701241202823622L;
	private GraphPDTDemo view;
	
	public ResetLayout(GraphPDTDemo view) {
		super("Reset Layout");
		this.view = view;
	}

	@Override
	public void actionPerformed(ActionEvent arg0) {
		view.calcLayout();
		
		
	}

}
