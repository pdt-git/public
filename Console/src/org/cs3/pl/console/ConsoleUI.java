/*
 */
package org.cs3.pl.console;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;

/**
 * specifies requirements for a console ui.
 * 
 * This is a bit like "The Humble Dialog Box" by M. Feather. Huhu Daniel! :-)
 * 
 * The implementation of these methods should be super-thin!
 */
public interface ConsoleUI {
    public void appendOutput(String text);

    public void setLineBuffer(String text);

    public void initUI(Composite parent);

    public void setFocus();
    
    public void setController(ConsoleController c);

    /**
     * @return the display or null, if not available.
     */
    public Display getDisplay();

	/**
	 * @return the position of the carret in the line buffer
	 */
	public int getCaretPosition();

	/**
	 * @param caretPosition
	 */
	public void setCaretPosition(int caretPosition);

	public void setSingleCharMode(boolean b);

}