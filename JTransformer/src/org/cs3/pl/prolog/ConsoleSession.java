package org.cs3.pl.prolog;

/**
 * @author degenerl
 */

public interface ConsoleSession extends PrologSession {
	/**
	 * attach the console object to the Session. This method 
	 * must only be called once.
	 * @param c an IConsole Object.
	 * @throws IllegalStateException if called twice or more.
	 */
	
	public abstract void initialise(IConsole c);
}