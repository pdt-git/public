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

	public boolean isUp();
	public boolean isDown();
	
	public void addLifeCycleHook(LifeCycleHook h);
	public void addLifeCycleHook(LifeCycleHook hook, String id, String[] dependencies);

    
}