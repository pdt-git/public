package org.cs3.pl.prolog;

import java.io.IOException;

public interface IPrologInterface {
	/**
	 * returns an instance of the "default" session. This is actually a call to
	 * getSession(Class) with the default class object as an argument, and
	 * therefore needs reflection and an (int)-constructor.
	 * 
	 * @return a new Session Object
	 */
	public abstract PrologSession getSession();

	/**
	 * causes complete re-initialization of the Prolog system, and invalidates
	 * all current sessions.
	 * 
	 * @throws IOException
	 */
	public abstract void stop() throws IOException;
	
	/**
	 * causes complete re-initialization of the Prolog system, and invalidates
	 * all current sessions.
	 * 
	 * @throws IOException
	 */
	public abstract void start() throws IOException;

	/**
	 * adds a system initialization callback. The method onInit() is called from
	 * each of them, with a specific initialization session. They are all
	 * executed, synchronously, before any other access is allowed.
	 * 
	 * <b>note: </b> adding an init hook while the PI is running is an error
	 * (there is no way to guarantee the expected execution context as described
	 * above).
	 * 
	 * @param i
	 *            an InitHook instance
	 * @throws an
	 *             IllegalStateException if the PI is already running.
	 */
	public void addInitHook(InitHook i) ;

	/**
	 * adds a startup callback. These callbacks are called synchronously before
	 * the system shuts down.
	 * 
	 * @param i
	 *            an StartupHook instance.
	 */
	public void addShutdownHook(ShutdownHook i) ;
	/**
	 * adds a startup callback. These callbacks are called asynchronously after
	 * system initialization.
	 * 
	 * @param i
	 *            an StartupHook instance.
	 */
	public void addStartupHook(final StartupHook i) ;
}