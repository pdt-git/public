package org.cs3.pl.prolog;

/**
 * Provides support for exclusive queries to the prolog System. The 
 * checkPrologAccess method must be called by each PrologSession
 * implementation to ensure no queries can be initiated while an 
 * exclusive query is active on the system.
 * @author schulzs1
 */
public class PrologSynchronizer {
	
	private boolean locked = false;
	
	private static PrologSynchronizer instance = new PrologSynchronizer();
	
	private PrologSynchronizer(){
		
	}

	/**
	 * returns the synchronization manager object.
	 * @return
	 */

	static public PrologSynchronizer getInstance(){
		return instance;
	}
	
	/**
	 * checks if a query lock is active, and throws an exception to
	 * prevent illegal access if such a lock has been detected.
	 * 
	 * @throws SessionException a query lock is active
	 */

	synchronized public void checkPrologAccess() throws SessionException{
		if (locked)
			throw new SessionException("Can't access Prolog due to query lock");
	}
	
	/**
	 * locks the system for the current query. No calls to query() or next() 
	 * methods are allowed util it is released. If a query is already
	 * being held by another thread, the method waits for that lock to
	 * be released.
	 * 
	 * @throws InterruptedException waiting for lock release fails.
	 * @throws SessionException the query is already locked.
	 */
		
	synchronized public void lock() throws InterruptedException, SessionException {
		if (locked)
			throw new SessionException("Already locked");
		
		locked = true;
	}
	
	/**
	 * unlocks the system. normal queries are allowed again once this method 
	 * returns
	 */
	
	synchronized public void unlock() {	
		locked = false;
		notify();
	}
	/**
	 * checks if there is currently a lock established on the
	 * PrologSychronizer.
	 * 
	 * @return true if the Synchronizer is locked
	 */
	
	synchronized public boolean isLocked() {
		return locked;
	}
}
