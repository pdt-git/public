package org.cs3.pl.console.prolog;

public interface PrologConsoleService {
	public void registerPrologConsole(PrologConsole console);
	public void unregisterPrologConsole(PrologConsole console);
	public PrologConsole[] getRegisteredPrologConsoles();
	public PrologConsole getActivePrologConsole();
}
