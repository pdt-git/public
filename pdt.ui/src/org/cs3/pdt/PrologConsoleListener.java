package org.cs3.pdt;

import java.util.EventListener;

public interface PrologConsoleListener extends EventListener{
	public void consoleRecievedFocus(PrologConsoleEvent e);
	public void consoleLostFocus(PrologConsoleEvent e);
	public void consoleVisibilityChanged(PrologConsoleEvent e);
}
