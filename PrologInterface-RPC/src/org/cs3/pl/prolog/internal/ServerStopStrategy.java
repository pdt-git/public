package org.cs3.pl.prolog.internal;

/**
 * howto stop a server running at a specified port.
 */
public interface ServerStopStrategy {
	public void stopServer(int port, boolean now);
}
