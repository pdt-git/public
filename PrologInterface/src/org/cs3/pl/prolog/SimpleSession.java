package org.cs3.pl.prolog;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.common.Debug;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingClientLoggingWrapper;

/**
 * SimpleSession is the basic Session. It does not provide any feedback to the user, but
 * provides a small, yet effective way of interacting with the prolog System. This is
 * the default implementation of getSession() in PrologInterface
 * @author terra
 * @inheritDoc
 */
public class SimpleSession implements PrologSession{	
	private ReusableClient rpcDelegate =null;
	
	protected boolean disposed;
	
	protected boolean queryActive = false;

	private ReusablePool connectionPool;
	
	/**
	 * constructs a SimpleSession. The Server is connected on the local host, at the
	 * passed port.
	 * @param port the port to connect to
	 * @throws IOException a connection failure occurs.
	 */

	public SimpleSession(int port) throws IOException{	
		rpcDelegate=new ReusableClient();
		rpcDelegate.enableLogging(new Logger("default"));
		rpcDelegate.configure("localhost", port);
		
		Debug.info("Client configured, connecting to server");
		rpcDelegate.start();
		
	}
	public SimpleSession(ReusableClient connection){
		rpcDelegate=connection;
		Debug.info("Session created, reusing existing connection");
		
	}

	synchronized public void dispose() {
		Debug.debug("disposed called");
		if (!disposed){
		    queryActive=false;
		   disposed=true;
		   if(connectionPool!=null){
		   	connectionPool.recycle(rpcDelegate);
		   }
		}
	}
	
	synchronized public Hashtable query(String query) throws SessionException{
	    if (disposed){
			throw new IllegalStateException("Session is disposed");
		}
		
		PrologSynchronizer.getInstance().beginAccess();
		
		try {
		    return doQuery(query);
		} finally {
		    PrologSynchronizer.getInstance().endAccess();
		}
	}
	
	synchronized public Hashtable next() throws SessionException {
	    if (disposed)
	        throw new IllegalStateException("Session is disposed");
	    
	    PrologSynchronizer.getInstance().beginAccess();
	    
	    try {
		    return doNext();
		} finally {
		    PrologSynchronizer.getInstance().endAccess();
		}
	}

	synchronized protected Hashtable doQuery(String query) throws SessionException {
		
		if (queryActive){
			Debug.debug("Implicitly ending query");
			endQuery();
		}
		
		queryActive = true;
		
		Hashtable rv;
		
		try {
			rv = (Hashtable) getRpcDelegate().call("RemotePrologSession", "query", new Object[]{query});			
		} catch (Exception e) {			
			SessionException ex = new SessionException("Error while executing call " + query);
			ex.initCause(e);
			throw ex;
		}
		
		if (rv == null )
			queryActive = false;
		
		return rv;
	}

	synchronized protected Hashtable doNext() throws SessionException {		
		if (!queryActive)
			throw new SessionException("No query is currently active");
				
		Hashtable rv;
		
		try {
			rv = (Hashtable) getRpcDelegate().call("RemotePrologSession", "next", new Object[]{});			
		} catch (Exception e) {
			SessionException ex  = new SessionException("Error while executing call");
			ex.initCause(e);
			throw ex;
		}
		
		if (rv == null)
			queryActive = false;
		
		return rv;
	}

	
	public void endQuery() throws SessionException {
		if (disposed)
			throw new IllegalStateException("Session is disposed");
			
		if (!queryActive)
			return;
		
		queryActive = false;	
	}
	
	public boolean consult(String name) {
		boolean windowsPlattform = System
	    .getProperty("os.name").indexOf("Windows") > -1;
		if (windowsPlattform)
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
	
   	
	/**
	 * @return Returns the connectionPool.
	 */
	public ReusablePool getConnectionPool() {
		return connectionPool;
	}
	/**
	 * @param connectionPool The connectionPool to set.
	 */
	public void setConnectionPool(ReusablePool connectionPool) {
		this.connectionPool = connectionPool;
	}
	/**
	 * @param rpcDelegate The rpcDelegate to set.
	 */
	public void setRpcDelegate(ReusableClient rpcDelegate) {
		this.rpcDelegate = rpcDelegate;
	}
	protected MessagingClientLoggingWrapper getRpcDelegate() {
		return rpcDelegate;
	}
	
}
