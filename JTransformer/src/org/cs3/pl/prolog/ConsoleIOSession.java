/*
 * Created on 06.07.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package org.cs3.pl.prolog;

import java.io.IOException;
import java.util.Hashtable;

import org.cs3.pl.Debug;
import org.rapla.components.rpc.Logger;
import org.rapla.components.rpc.MessagingClientLoggingWrapper;

/**
 * @author degenerl
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class ConsoleIOSession extends MessagingClientLoggingWrapper implements ConsoleSession {

	private long consoleHandle;
	private long prologEngine;
	
	private Thread prologThread;
	private IConsole console;
	
	private byte [] helperArray;
	private int mode = 0;
	
	public ConsoleIOSession(int port) throws IOException{
		enableLogging(new Logger("default"));
		configure("localhost", port);
		
		Debug.info("Client configured, connecting to server");
		start();
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#dispose()
	 */
	public void dispose() {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
	 */
	public Hashtable query(String query) throws SessionException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#next()
	 */
	public Hashtable next() throws SessionException {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#endQuery()
	 */
	public void endQuery() {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#consult(java.lang.String)
	 */
	public boolean consult(String name) {
		// TODO Auto-generated method stub
		return false;
	}

	public void initialise(IConsole c) {	
		console = c;
		
		prologEngine = createEngine();
		consoleHandle = createConsoleStruct();

		long inHandle =  createStreamInC(consoleHandle, IConsole.IN_STREAM);
		long outHandle = createStreamInC(consoleHandle, IConsole.OUT_STREAM);
		long errHandle = createStreamInC(consoleHandle, IConsole.ERR_STREAM);
		
		long termHandleIn = unifyWithTerm(inHandle);
		long termHandleOut = unifyWithTerm(outHandle);
		long termHandleERR = unifyWithTerm(errHandle);
		
		setStream(termHandleIn, "alias(user_input)");
		setStream(termHandleOut, "alias(user_output)");
		setStream(termHandleERR, "alias(user_error)");
		setStream(termHandleIn, "alias(current_input)");
		setStream(termHandleOut, "alias(current_output)");
		setStream(termHandleERR, "alias(current_error)");
	}	
	

	public void finalize() {
		deleteConsoleStruct(consoleHandle);		
		deleteEngine(prologEngine);
		stop();
	}
	
	synchronized private int writeHelper(int stream) throws IOException {
		return console.write(stream, helperArray);
	}
	
	synchronized private int readHelper(int size) throws IOException{
		helperArray = new byte[size];
		
		return console.read(helperArray, mode);	
	}
	
	private native long createEngine();
	private native long createConsoleStruct();
	private native long createStreamInC(long handle, int fdnr);
	private native long unifyWithTerm(long handle);
	
	private native void setStream(long handle, String val);
	private native void deleteConsoleStruct(long ptr);
	private native void deleteEngine(long ptr);

	/* (non-Javadoc)
	 * @see org.cs3.pl.prolog.PrologSession#isDisposed()
	 */
	public boolean isDisposed() {
		// TODO Auto-generated method stub
		return false;
	}
}
