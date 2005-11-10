package org.cs3.pdt.runtime;

import java.util.EventListener;

public interface PrologContextTrackerListener extends EventListener {
	public void contextChanged(PrologContextTrackerEvent e);
}
