package org.cs3.pl.prolog;

import java.util.Hashtable;

/**
 * Provides an interaction interface with the Prolog System. These methods should
 * provide a way to pass queries to the Prolog engine, and retrieve results. Currently
 * all PrologSession implementations <u>must</u> provide a constructor that takes a 
 * single int as argument.
 * 
 * @author terra
 */
public interface PrologSession {

	/**
	 * Disposes the session. Any further call (except of further dispose calls
	 * will cause the system to throw an IllegalStateException. 
	 */
	public void dispose();
	
	/**
	 * begins a query on the Prolog system. If there was an active query running, further
	 * results lost, so calls to next() will return results of this query. The
	 * resultant Hashtable contains keys equal to the unbound variables in the query,
	 * and values equal to their binding. If null is returned, no bindings could
	 * satisfy the query ("no"). If there were no unbound variables, an empty Hashtable
	 * is returned to signify "yes".
	 *
	 * Each Query <u>must</u> call the method checkPrologAccess() of the class
	 * PrologSynchronizer before attempting any interaction with the Prolog
	 * System. If there is an exclusive query active, the implementation
	 * must rethrow the resultant PrologException if it is caught.
	 *   
	 * @return a hashtable, containing the bindings generated.
	 * @param query a prolog query
	 * @throws IllegalStateException the session is disposed
	 * @throws PrologException an abnormal condition was detected
	 */
	
	public Hashtable query(String query) throws PrologException;
	public Hashtable[] queryAll(String query)throws PrologException;
	/**
	 * returns the next set of Bindings satisfying the last query.
	 * 
	 * Each Query <u>must</u> call the method checkPrologAccess() of the class
	 * PrologSynchronizer before attempting any interaction with the Prolog
	 * System. If there is an exclusive query active, the implementation
	 * must rethrow the resultant PrologException if it is caught.
	 * 
	 * @return another set of Bindings
	 * @throws IllegalStateException the session is disposed
	 * @throws PrologException an IO Error occured
	 */
	
	public Hashtable next() throws PrologException;
		
	/**
	 * explicitly ends the last query, discarding further results if any existed.
	 * If no query was active, this is a noop.
	 * 
	 * @throws IllegalStateException the session is disposed
	 * @throws PrologException a lower-level failure has occured while killing
	 * the query.
	 */
	
	public void endQuery() throws PrologException;
	
	/**
	 * consults the File signified by the path passed into the Prolog System. Typically
	 * implemented by a query. The return value is a "best effort" guess, since several
	 * failed directives do not necessarly prompt Prolog to consider the overall consultation
	 * failed. Consulted facts are (of course) globally valid for all sessions.
	 * 
	 * @return true if the consultation seems to have succeeded, false if it definitly failed
	 * @param name a filename
	 * @throws IllegalStateException the session is disposed
	 */
	
	public boolean consult(String name)throws PrologException;

	/**
	 * checks if the session has been disposed. This can happen without the users explicitly
	 * calling dispose, if for example restart() is called on the PrologInterface.
	 * @return true if the interface has been disposed, false otherwise
	 */
	public boolean isDisposed();

}
