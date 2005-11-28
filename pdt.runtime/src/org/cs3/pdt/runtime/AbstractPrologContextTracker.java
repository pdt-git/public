package org.cs3.pdt.runtime;

import java.util.Iterator;
import java.util.Vector;

/**
 * Convenience class for implementing PrologContextTracker.
 * 
 * Subclasses need to provide implementations for init() and
 * getCurrentPrologInterface(). In addition, subclasses are responsible for
 * calling fireContextChanged() when apropiate, i.e. when they think that the
 * "current" PrologInterface has changed, become available or invalid.
 * 
 * Clients that want to register a static PrologContextTracer instance via the
 * extension point prologContextTracker *MUST* subclass this class.
 * 
 * Other clients are free to use this class as a starting point.
 * 
 * @author lukas
 * 
 */
public abstract class AbstractPrologContextTracker implements
		PrologContextTracker {

	private String id;

	private String label;

	private Vector listeners = new Vector();

	/**
	 * Notify listeners that the "current" PrologInterface has changed.
	 * 
	 * Subclasses should call this method whenever they think the "current"
	 * PrologInterface has changed, become available or invalid.
	 */
	protected void fireContextChanged() {

		PrologContextTrackerEvent e = new PrologContextTrackerEvent(this,
				getCurrentPrologInterface());
		Vector cloned = null;
		synchronized (listeners) {
			cloned = (Vector) listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PrologContextTrackerListener l = (PrologContextTrackerListener) it
					.next();
			l.contextChanged(e);
		}
	}

	public void addPrologContextTrackerListener(PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}

	}

	public String getLabel() {
		return label;
	}

	public String getId() {
		return id;
	}

	public AbstractPrologContextTracker() {
		id = null;
		label = null;
	}

	public AbstractPrologContextTracker(String id, String label) {
		this.id = id;
		this.label = label;
	}

	public void setLabel(String label) {
		this.label = label;

	}

	public void setId(String id) {
		this.id = id;
	}

}
