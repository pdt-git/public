package pdt.y.view.actions;

import java.awt.event.ActionEvent;

import javax.swing.AbstractAction;

/**
 * Action that terminates the application
 */
public  class ExitAction extends AbstractAction {
	/**
	 * 
	 */
	private static final long serialVersionUID = 96864293273482994L;

	public ExitAction() {
		super("Exit");
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		System.exit(0);
	}
}