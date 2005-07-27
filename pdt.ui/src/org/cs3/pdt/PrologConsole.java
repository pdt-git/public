package org.cs3.pdt;

import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.prolog.PrologInterface;

public interface PrologConsole {
	public ConsoleModel getModel();
	public PrologInterface getPrologInterface();
	public void addPrologConsoleListener(PrologConsoleListener l);
	public void removePrologConsoleListener(PrologConsoleListener l);
	public boolean isVisible();
}
