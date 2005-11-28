package org.cs3.pdt.runtime;

import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.ui.IWorkbench;

/**
 * keeps track of "the current" PrologInterface.
 * 
 * As there might be several (conflicting) notions or strategies of determining
 * the "active" or "current" PrologInterface at a given point in time, I decided
 * to attach a name to each of this strategies and let the user decide which one
 * to use.
 * 
 * Clients who are interested in finding out or following the currently "active"
 * PrologInterface should check with the ContextTrackerService to obtain a list
 * of all contributed strategies and either choose one that seems fitting or
 * even beter: ask the user.
 * 
 * Clients can overide this Interface to contribute their own
 * PrologContextTracker strategy and make it available to other clients by
 * registering it at the PrologRuntimePlugin's ContextTrackerService. Typically
 * this is done by extending the extension point prologContextTracker of the
 * pdt.runtime plugin. 
 * 
 * @author lukas
 * 
 */
public interface PrologContextTracker {
	/**
	 * add a listener.
	 * 
	 * Note: In most cases, listeners should be registered with the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * The listener will be informed whenever the tracker thinks that the active
	 * PrologInterface has changed, becomes available or invalid.
	 * 	 * 
	 * Implemntations are responsible for calling the listener methods.
	 * 
	 * 
	 * @param l
	 */
	public void addPrologContextTrackerListener(PrologContextTrackerListener l);

	/**
	 * remove a listener.
	 * 
	 * Note: In most cases, listeners should be unregistered from the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * @param l
	 */
	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l);

	/**
	 * @return a descriptive label for this tracker. This will be presented to
	 *         the user.
	 */
	public String getLabel();

	/**
	 * @return a unique identifier for this tracker.
	 */
	public String getId();

	/**
	 * There is no current PrologInterface.
	 * find out what this tracker THINKS is the currently active PrologInterface.
	 * 
	 * @return the PrologInterface or null, if the tracker thinks that none is
	 *         active.
	 */
	public PrologInterface getCurrentPrologInterface();

	/**
	 * initialize the tracker.
	 * 
	 * This method is called by the pdt.runtime plugin at the earliest possible
	 * time that satisfies the following conditions: - the tracker has been
	 * registered with the plugin's tracker service - the eclipse workbench ui
	 * is up and running.
	 * 
	 * Implementation should seize the oportunity to attach to parts of the
	 * workbench ui they might be interested in.
	 * 
	 * @param workbench
	 */
	public void init(IWorkbench workbench);
}
