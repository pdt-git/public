package org.cs3.pl.prolog;


/**
 * hook into the PIFs lifecycle.
 */
public interface LifeCycleHook{
    /**
     * called by the PrologInterface during startup phase.
     * <br>
     * This method executes on the same thread that starts the prolog interface.
     * No interaction via regular PrologSessions will take place before
     * this method is called on all registered LifeCycleHooks.
     * <br><b>Important Note:</b> Only access the pif through the
     * initSession argument. In particular, take care that this method
     * does not indirectly trigger a call to PrologInterface.getSession() on the same thread,
     * or you will very propably couse a dead lock. The initial session cannot be 
     * disposed.  
     * @param initSession safe-mode session for startup phase.
     */
	abstract void onInit(PrologSession initSession);
	
	/**
     * called by the PrologInterface  after the startup is complete.
     * <br>
     * This method executes asynchronously to the thread that started
     * the prolog interface. By the time it is called, the pif is guaranteed to be
     * up and ready for normal operation (getSession() and friends).
     * <br>
     * 
     */	
	abstract void afterInit();
	
	/**
     * called by the PrologInterface  before the pif shuts down.
     * <br>
     * This method is called on the same thread that stops the prolog interface.
     * There are no other sessions running. (FIXME verify this!!)
     * <br><b>Important Note:</b> Only access the pif through the
     * initSession argument. In particular, take care that this method
     * does not indirectly trigger a call to PrologInterface.getSession() on the same thread,
     * or you will very propably couse a dead lock. The cleanup session cannot be 
     * disposed.  
     */		
	abstract void beforeShutdown(PrologSession session);	
}
