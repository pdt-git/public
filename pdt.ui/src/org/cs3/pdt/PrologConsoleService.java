package org.cs3.pdt;

public interface PrologConsoleService {
	public void registerPrologConsole(PrologConsole console);
	public void unregisterPrologConsole(PrologConsole console);
	public PrologConsole[] getRegisteredPrologConsoles();
	public PrologConsole getActivePrologConsole();
}
