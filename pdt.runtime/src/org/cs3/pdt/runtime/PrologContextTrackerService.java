package org.cs3.pdt.runtime;

public interface PrologContextTrackerService {
	/**
	 * register a context tracker with the runtime plugin.
	 * Any listeners that have already subscribed to the trackers id will be added.
	 * @param tracker
	 */
	public void registerPrologContextTracker(PrologContextTracker tracker);
	
	/**
	 * unregister a context tracker from the runtime plugin.
	 * Listeners that were subscribed through the runtime plugin's global listener registry
	 * will be removed from the tracker.
	 * @param tracker
	 */
	public void unregisterPrologContextTracker(PrologContextTracker tracker);
	/**
	 * registers a listener for a particular tracker at the earliest possible point in time.
	 * If the tracker is already registered, the listener is added at once. Otherwise it will be 
	 * added when the tracker registeres.
	 * @param trackerID
	 * @param l
	 */
	public void addPrologContextTrackerListener(String trackerID, PrologContextTrackerListener l);
	
	/**
	 * unregisters a listener from a particular tracker id.
	 * If the tracker is registered, the listener is removed. It is also removed from the plugins
	 * global listener table and will not be added to any other tracker registering with the same 
	 * tracker id in the future.
	 * 
	 * In othe words, this undoes the effect of addPrologContextTrackerListener(String,PrologContextListener)
	 * @param trackerID
	 * @param l
	 */
	public void removePrologContextTrackerListener(String trackerID, PrologContextTrackerListener l);
	
	/**
	 * @return an array containing all context trackers currently registered with the runtime plugin.
	 */
	public PrologContextTracker[] getContextTrackers();

	public PrologContextTracker getContextTracker(String trackerId);
}
