package org.cs3.pl.prolog;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.Vector;

import org.cs3.pl.common.Debug;

/**
 * Implementation for the DirectAccessSession interface in the client/server architecture.
 * This implementation should be very basic, passing whatever possible to a normal PrologSession
 * Object, and only implementing the new functionalities.
 * 
 * @author terra
 */
public class StreamBackedSession extends SimpleSession implements DirectAccessSession {

	private Vector listeners;
	private String lastQuery;
	
	private int port;
	private OutputStream print;
	
	/**
	 * Constructs a new StreamBackedSession. Since this is not a regular session,
	 * it has a non-matching constructor and can not be created by getSession(Class)
	 * in PrologInterface. This class should only and exclusively be used in the 
	 * PrologConsole, and <u>nowhere</u> but there.
	 * 
	 * @param port port to connect to.
	 * @param print the stream characters can be written to
	 * @throws IOException 
	 */
	
	StreamBackedSession(int port, OutputStream print) throws IOException  {
		super(port);
		this.print = print;
		this.port = port;
	}

	public void sendChar(char c) throws SessionException {
		if (disposed)
			throw new SessionException("Session is disposed");
		
		try {
			print.write(c);
			print.flush();
		} catch (IOException e){
			throw new SessionException("IOException occured while writing to Server", e);
		}
	}
	
	public void endQuery() throws SessionException {
	    if (!queryActive)
	        return;
	    
	    super.endQuery();
	    
	    try {
	        StreamBackedSession sbs = new StreamBackedSession(port, print);
	        sbs.getRpcDelegate().call("RemotePrologSession", "abort", new Object[]{lastQuery});
	        sbs.dispose();
        } catch (Exception e){
            throw new SessionException("Aborting active query failed", e);
        }
	}
	
	synchronized public Hashtable query(String query) throws SessionException{
	    lastQuery = query;
	    return super.query(query);
	}

	public void sendChars(String s) throws SessionException {
        if (disposed) { throw new SessionException("Session is disposed"); }

        try {
            print.write(s.getBytes());
            print.flush();
        } catch (IOException e) {
            throw new SessionException(
                    "IOException occured while writing to Server", e);
        }

    }

    public Hashtable exclusiveQuery(String query) throws SessionException {
        PrologSynchronizer pls = PrologSynchronizer.getInstance();

        if (disposed) throw new IllegalStateException("Session is disposed");
        
        synchronized (this){
            lastQuery = query;
        }

        try {
            pls.lock();
            return doQuery(query);
        } catch (InterruptedException e) {
            Debug.report(e);
            throw new SessionException("Interrputed while executing query", e);
        } finally {
            pls.unlock();
        }
    }

    public Hashtable exclusiveNext() throws SessionException {
        PrologSynchronizer pls = PrologSynchronizer.getInstance();
        Hashtable rv;

        if (disposed) throw new IllegalStateException("Session is disposed");

        try {
            pls.lock();

            return doNext();
        } catch (InterruptedException e) {
            Debug.report(e);
            throw new SessionException("Interrputed while executing next", e);
        } finally {
            pls.unlock();
        }
    }

    public boolean isQueryActive() {
        return queryActive;
    }
}