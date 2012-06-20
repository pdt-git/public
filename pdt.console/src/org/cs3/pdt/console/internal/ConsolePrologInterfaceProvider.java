package org.cs3.pdt.console.internal;

import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.service.PrologInterfaceProvider;

public class ConsolePrologInterfaceProvider implements PrologInterfaceProvider {
	
	@Override
	public int getPriority() {
		return 1000;
	}
	
	@Override
	public PrologInterface getPrologInterface() {
		PrologConsole activePrologConsole = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		if (activePrologConsole == null) {
			return null;
		} else {
			return activePrologConsole.getPrologInterface();
		}
	}
	
}
