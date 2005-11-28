package org.cs3.pl.console.prolog;

import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.prolog.PrologInterface;

public interface PrologConsole {
	public ConsoleModel getModel();
	public PrologInterface getPrologInterface();
	public void setPrologInterface(PrologInterface pif);
	public void addPrologConsoleListener(PrologConsoleListener l);
	public void removePrologConsoleListener(PrologConsoleListener l);
	public boolean isVisible();
	public String getText();
	public int getLineAtOffset(int offset);
	public int getOffsetAtLine(int line);
	public int getLineCount();
	public void clearOutput();
	public int getCaretOffset();
	public String getTextRange(int offset, int length);
	
}
