package org.cs3.pl.prolog;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.Debug;

/**
 * Implementation for the DirectAccessSession interface in the client/server architecture.
 * This implementation should be very basic, passing whatever possible to a normal PrologSession
 * Object, and only implementing the new functionalities.
 * 
 * @author terra
 */
public class StreamBackedSession extends SimpleSession implements DirectAccessSession {
	
	private static class PrologReaderRunnable implements Runnable {
		
		private BufferedReader reader;
		private Vector listeners;

		public PrologReaderRunnable(InputStream reader, Vector queue){
			this.reader = new BufferedReader(new InputStreamReader(reader));
			this.listeners = queue;
		}

		public void run() {
			try {
				while (true){
					String next = reader.readLine();
					Vector toProcess = (Vector) listeners.clone();
					
					for (Iterator i = toProcess.iterator(); i.hasNext();) {
						OutputListener ol = (OutputListener) i.next();
						ol.onOutput(next);
					}
					
				}
			} catch (IOException e) {
				Debug.error("Lost connection to server, no more output will be reported");
			}
		}
	}

	private OutputStream print;
	private Vector listeners;
	
	/**
	 * Constructs a new StreamBackedSession. Since this is not a regular session,
	 * it has a non-matching constructor and can not be created by getSession(Class)
	 * in PrologInterface. This class should only and exclusively be used in the 
	 * PrologConsole, and <u>nowhere</u> but there.
	 * 
	 * @param port port to connect to.
	 * @param printer the stream characters can be written to
	 * @param reader the stream characters can be read from
	 * @throws IOException 
	 */
	
	StreamBackedSession(int port, OutputStream printer, InputStream reader) throws IOException  {
		super(port);
		print = printer;
		listeners = new Vector();
		
		Thread readerThread = new Thread(new PrologReaderRunnable(reader, listeners));
		readerThread.setName("Prolog Reader Thread");
		readerThread.setDaemon(true);
		readerThread.start();
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

	public Hashtable exclusiveQuery(String query) throws SessionException {
		PrologSynchronizer pls = PrologSynchronizer.getInstance();
		
		if (disposed)
			throw new IllegalStateException("Session is disposed");
		
		
		
		if (!queryActive)
			throw new SessionException("No query is currently active");
		
		Hashtable rv;
		
		try {
			pls.lock();
						
			if (queryActive){
				Debug.debug("Implicitly ending query");
				endQuery();
			}
			
			queryActive = true;
			
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
		} catch (Exception e) {
			SessionException ex  = new SessionException("Error while executing call");
			ex.initCause(e);
			throw ex;
		} finally {
			pls.unlock();		}
		
		
	}

	public Hashtable exclusiveNext() throws SessionException {
		PrologSynchronizer pls = PrologSynchronizer.getInstance();
		Hashtable rv;
		
		if (disposed)
			throw new IllegalStateException("Session is disposed");
		
		
		try {
			
			if (!queryActive)
				throw new SessionException("No query is currently active");
					
			rv = (Hashtable) call("PrologSession", "next", new Object[]{});
			 
			if (rv == null)
				queryActive = false;
			
			return rv;
		} catch (Exception e) {
			SessionException ex  = new SessionException("Error while executing call");
			ex.initCause(e);
			throw ex;
		} finally {
			pls.unlock();	
		} 
	}

	public void addOutputListener(OutputListener out) {
		listeners.add(out);
	}

	public void removeOutputListener(OutputListener out) {
		listeners.remove(out);
	}

}
