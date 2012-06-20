package org.cs3.prolog.pif.service;

import org.cs3.prolog.pif.PrologInterface;

public interface PrologInterfaceProvider {

	int getPriority();
	
	PrologInterface getPrologInterface();
	
}
