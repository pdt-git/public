package org.cs3.pdt.runtime;

import java.util.Iterator;
import java.util.Vector;

public abstract class AbstractPrologContextTracker implements
		PrologContextTracker {

	private String id;

	private String label;

	private Vector listeners = new Vector();

	

	protected void fireContextChanged(){
		
		PrologContextTrackerEvent e = new PrologContextTrackerEvent(this, getCurrentPrologInterface());
		Vector cloned = null;
		synchronized (listeners) {
			cloned=(Vector) listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PrologContextTrackerListener l = (PrologContextTrackerListener) it.next();
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

	
	public AbstractPrologContextTracker(){
		id=null;
		label=null;
	}
	public AbstractPrologContextTracker(String id, String label) {
		this.id = id;
		this.label = label;
	}

	public void setLabel(String label) {
		this.label=label;
		
	}
	public void setId(String id){
		this.id=id;
	}

}
