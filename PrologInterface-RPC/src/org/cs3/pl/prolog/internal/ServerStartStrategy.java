package org.cs3.pl.prolog.internal;

/**
 * A "HOWTO-start-the-Server"-Strategie.
 *
 * This is a (temporary?) trick to eliminate dependencies 
 * of the PrologInterface on any framework specific components 
 * (PDTPlugin).
 * 
 * The Idea: give the PI a "harmless" (static?) implemntation as default.
 * The PDTPlugin may, once it exists and is up and running,
 * provide its own  (more sensible) implementation.
 * 
 * So we can effectivly turn the dependency arround. Its the plugin level 
 * that now depends on the service level (which is ok) not vice versa 
 * (which would be bad, ilogical, unintuitive, malicious, wicked, etc.).
 * 
 * neat, isn't it.
 * 
 * @author degenerl
 * 
 */
public interface ServerStartStrategy {
	/**
	 * starts the server process, returning its process.
	 * @return the server process
	 */
	public Process startServer(int port);
}
