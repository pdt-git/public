package org.cs3.pdt.core;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.runtime.IAdaptable;

public interface PEFHandle extends IAdaptable{
	public String getId();
	
	public PrologInterface getPrologInterface();
}
