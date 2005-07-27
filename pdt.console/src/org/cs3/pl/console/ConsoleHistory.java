package org.cs3.pl.console;
/**
 * the history is just another view/controller. 
 * it operates directly on the model.
 */
public interface ConsoleHistory {
	public void setConsoleModel(ConsoleModel model);
	public ConsoleModel getConsoleModel();
	public void previous();
	public void next();
	public void clearHistory();
	 
}
