package org.cs3.pl.prolog;

/**
 * howto stop a server running at a specified port.
 */
public interface ServerStopStrategy {
	public void stopServer(int port, boolean now);
}
