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
	private int using = 0;
	
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
	 * locks the system for the current query. No calls to query() or next() 
	 * methods are allowed util it is released. If a query is already
	 * being held by another thread, the method waits for that lock to
	 * be released.
	 * 
	 * @throws InterruptedException waiting for lock release fails.
	 */
		
	public void lock() throws SessionException, InterruptedException {
	    
	    synchronized (this){
	        if (locked)
	            throw new SessionException("Can't access Prolog due to query lock");
	        
	        locked = true;
	        
	        while (using > 0){
	            wait();                
	        }
	    }
	}
	
	/**
	 * unlocks the system. normal queries are allowed again once this method 
	 * returns
	 */
	
	synchronized public void unlock() {	
		locked = false;
	}
	
	/**
	 * checks if there is a lock. 
	 * @return true if the Synchronizer is locked.
	 */
	
	synchronized public boolean isLocked() {
		return locked;
	}

    synchronized public void beginAccess() throws SessionException {
        if (locked)
            throw new SessionException("Prolog is Locked");
        
        using++;
    }
    
    synchronized public void endAccess(){
        using--;
        
        if (using == 0)
            notify();
    }
}
