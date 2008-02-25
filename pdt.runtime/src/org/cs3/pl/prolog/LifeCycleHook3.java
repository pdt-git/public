package org.cs3.pl.prolog;

public interface LifeCycleHook3 extends LifeCycleHook2 {
	/**
	 * called by the PrologInterface when the hook is registered while the pif is already up or in the process of starting up.
	 * When it is called, the PrologInterface is up and running.
	 * Note that this method will ignore hook dependencies. 	
	 * 
	 * Most implementation will just call onInit and/or after init, since they will not 
	 * be called by the PrologInterface when the hook is registered "to late", i.e. after the
	 * startup sequence has begun.
	 * 
	 */
	public void lateInit(PrologInterface pif);
}
