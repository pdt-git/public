package org.cs3.pl.prolog;

/**
 * represents a startup callback. Startup callbacks are asynchronously called once the
 * PrologInterface (re)starts.
 * @author terra
 */
public interface StartupHook {
	
	/**
	 * called once a PrologInterface (re)starts. It must create its own sessions, which
	 * is possible once execution reaches this point.
	 */
	public void onStartup();
}
