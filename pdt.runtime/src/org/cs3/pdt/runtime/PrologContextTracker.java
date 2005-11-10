package org.cs3.pdt.runtime;

import org.cs3.pl.prolog.PrologInterface;

public interface PrologContextTracker {
	public void addPrologContextTrackerListener(PrologContextTrackerListener l);
	public void removePrologContextTrackerListener(PrologContextTrackerListener l);
	public String getLabel();
	public String getId();
	public PrologInterface getCurrentPrologInterface();
}
