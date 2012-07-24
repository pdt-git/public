/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.console;

import org.cs3.prolog.pif.PrologInterface;

public interface PrologConsole {
	public ConsoleModel getModel();
	public PrologInterface getPrologInterface();
	public void setPrologInterface(PrologInterface pif);
	public boolean isVisible();
	public String getText();
	public int getLineAtOffset(int offset);
	public int getOffsetAtLine(int line);
	public int getLineCount();
	public void clearOutput();
	public int getCaretOffset();
	public void setCaretOffset(int offset);
	public int getStartOfInput();
	public String getTextRange(int offset, int length);
	public void ensureConnectionForCurrentPrologInterface();
	
}

