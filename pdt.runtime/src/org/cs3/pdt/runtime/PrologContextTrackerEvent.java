package org.cs3.pdt.runtime;

import java.util.EventObject;

import org.cs3.pl.prolog.PrologInterface;

public class PrologContextTrackerEvent extends EventObject {

	private PrologInterface pif;

	/**
	 * creates a new PrologContextTrackerEvent.
	 * 
	 * 
	 * @param source
	 *            this should be the tracker that caused the event.
	 * @param pif
	 *            this should be what the pif thinks is the currently active
	 *            PrologInterface _AFTER_ the change. Maybe null to indicate
	 *            that no pif is currently active according to the source
	 *            tracker.
	 */
	public PrologContextTrackerEvent(Object source, PrologInterface pif) {
		super(source);
		this.pif = pif;
	}

	/**
	 * @return the currently active PrologInterface (according to the tracker
	 *         that send this event), or null if no PrologInterface is active
	 *         (dito).
	 */
	public PrologInterface getPrologInterface() {
		return pif;
	}
}
