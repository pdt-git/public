package org.cs3.pl.prolog;

import java.util.Hashtable;


/**
 * A Direct Acess Session is a subtype of the generic PrologSession that supports
 * sending single characters to a running query, and several other methods mainly
 * useful for implementing a console. (For example, if the query is being
 * debugged, the choices of the user must be communicated by single characters). This
 * kind of session may be sluggish or less performant. Unless the special functionality
 * is needed, the user should use generic PrologSession implementations. 
 * 
 * @author terra
 */
public interface DirectAccessSession extends PrologSession {
	/**
	 * sends a single character to the Prolog System. Of course, it
	 * will fail once the Session has been disposed.
	 * @param c the character to be send
	 * @throws SessionException any failure sending the character raises this exception
	 */
	public abstract void sendChar(char c) throws SessionException;
	
	/**
	 * adds a listener to the Prolog output stream. All output will be forwarded to
	 * the listner objects.
	 * @param out an OutputListener instance.
	 */	
	public abstract void addOutputListener(OutputListener out);
	
	/**
	 * removes a listener to the Prolog output stream.
	 * @param out an OutputListener instance.
	 */
	
	public abstract void removeOutputListener(OutputListener out);
	
	/**
	 * runs a query exclusively. This means that until this query is finished,
	 * and returns its result, no other Query may start. The method guarantees
	 * that the Prolog System exclusively processes this single query during
	 * the time.
	 * 
	 * If another query is started while an exclusive query is running, a 
	 * SessionException must be thrown.
	 * 
	 * @param query the Prolog Query to be evaluated, as a string.
	 * @return a hashtable with the keys equal to the bound variables, and the values
	 * equal to their bindings
	 * @throws SessionException an unexpected error condition occured
	 */
	
	public abstract Hashtable exclusiveQuery(String query) throws SessionException;
	
	/**
	 * returns the next result exclusively. No new queries may be initiated until this
	 * method returns, and all attempts to do so will cause a SessionException.
	 * 
	 * @return a new set of results
	 * @throws SessionException an exception occurred
	 */
	
	public abstract Hashtable exclusiveNext() throws SessionException; 
}
