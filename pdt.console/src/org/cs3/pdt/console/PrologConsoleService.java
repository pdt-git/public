/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.console;

public interface PrologConsoleService {
	public void registerPrologConsole(PrologConsole console);
	public void unregisterPrologConsole(PrologConsole console);
	public PrologConsole[] getRegisteredPrologConsoles();
	public PrologConsole getActivePrologConsole();
	public void addPrologConsoleListener(PrologConsoleListener l);
	public void removePrologConsoleListener(PrologConsoleListener l);

}

