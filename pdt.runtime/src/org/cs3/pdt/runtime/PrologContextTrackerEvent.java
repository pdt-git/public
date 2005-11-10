package org.cs3.pdt.runtime;

import java.util.EventObject;

import org.cs3.pl.prolog.PrologInterface;

public class PrologContextTrackerEvent extends EventObject {

	private PrologInterface pif;

	public PrologContextTrackerEvent(Object source,PrologInterface pif) {
		super(source);
		this.pif=pif;
	}

	public PrologInterface getPrologInterface(){
		return pif;
	}
}
