package org.cs3.pl.prolog;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.Debug;
import org.cs3.pl.SystemProperties;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingClientLoggingWrapper;

/**
 * SimpleSession is the basic Session. It does not provide any feedback to the user, but
 * provides a small, yet effective way of interacting with the prolog System. This is
 * the default implementation of getSession() in PrologInterface
 * @author terra
 * @inheritDoc
 */
public class SimpleSession extends MessagingClientLoggingWrapper implements PrologSession {	
	
	protected boolean disposed;
	protected boolean queryActive = false;
	
	/**
	 * constructs a SimpleSession. The Server is connected on the local host, at the
	 * passed port.
	 * @param port the port to connect to
	 * @throws IOException a connection failure occurs.
	 */

	public SimpleSession(int port) throws IOException{	
		enableLogging(new Logger("default"));
		configure("localhost", port);
		
		Debug.info("Client configured, connecting to server");
		start();
	}

	synchronized public void dispose() {
		if (!disposed){
			disposed = true;
			stop();
		}
	}

	synchronized public Hashtable query(String query) throws SessionException {
		if (disposed){
			throw new IllegalStateException("Session is disposed");
		}
		
		PrologSynchronizer.getInstance().checkPrologAccess();
		
		if (queryActive){
			Debug.debug("Implicitly ending query");
			endQuery();
		}
		
		queryActive = true;
		Hashtable rv;
		
		try {
			rv = (Hashtable) call("PrologSession", "query", new Object[]{query});
		} catch (Exception e) {			
			SessionException ex = new SessionException("Error while executing call");
			ex.initCause(e);
			throw ex;
		}
		
		if (rv == null || rv.size() == 0)
			queryActive = false;
		
		return rv;
	}

	synchronized public Hashtable next() throws SessionException {
		if (disposed)
			throw new IllegalStateException("Session is disposed");
		
		PrologSynchronizer.getInstance().checkPrologAccess();
		
		if (!queryActive)
			throw new SessionException("No query is currently active");
		
		Hashtable rv;
		
		try {
			rv = (Hashtable) call("PrologSession", "next", new Object[]{});
		} catch (Exception e) {
			SessionException ex  = new SessionException("Error while executing call");
			ex.initCause(e);
			throw ex;
		}
		
		if (rv == null)
			queryActive = false;
		
		return rv;
	}

	
	synchronized public void endQuery() {
		if (disposed)
			throw new IllegalStateException("Session is disposed");
			
		if (!queryActive)
			return;
		
		queryActive = false;
	}
	
	public boolean consult(String name) {
		if (new SystemProperties().isWindowsPlattform())
			name = name.replace('\\', '/');
		
		try {
			return query("consult('" + name +"')") != null;
		} catch (SessionException e){
			Debug.report(e);
		}
		
		return false;
	}
	
	public boolean isDisposed() {
		return disposed;
	}
	
	public void finalize() throws Throwable {
		if (!disposed){
			Debug.warning("Implicitly disposing Session in finalizer");
			dispose();
		}
	}

	
}
