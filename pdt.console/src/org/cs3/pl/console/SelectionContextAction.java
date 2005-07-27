package org.cs3.pl.console;

import org.eclipse.jface.action.IAction;
import org.eclipse.swt.custom.StyledText;

public interface SelectionContextAction extends IAction {

	/**
	 * Called at action initialization time,
	 * beore validate and run() have been called.
	 * 
	 * @param text
	 */
	
	public void init(StyledText text);

	/**
	 * Based on the result of validate
	 * the action will be enabled or disabled.
	 * 
	 * @return
	 */
	public boolean validate();


	/**
	 * Executes the action after user activation.
	 * 
	 * @return
	 */
public void run();

	
}
