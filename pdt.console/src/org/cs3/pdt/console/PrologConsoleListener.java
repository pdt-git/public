/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.console;

import java.util.EventListener;

public interface PrologConsoleListener extends EventListener{
	public void consoleRecievedFocus(PrologConsoleEvent e);
	public void consoleLostFocus(PrologConsoleEvent e);
	public void consoleVisibilityChanged(PrologConsoleEvent e);
	public void activePrologInterfaceChanged(PrologConsoleEvent e);
}

