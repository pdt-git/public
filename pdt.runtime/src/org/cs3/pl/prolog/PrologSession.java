package org.cs3.pl.prolog;

import java.io.InputStream;
import java.util.List;
import java.util.Map;

public interface PrologSession extends Disposable{

	/**
	 * retrieve the PrologInterface that created this session.	
	 */
	public PrologInterface getPrologInterface();
	

    /**
     * begins a query on the Prolog system. If there was an active query
     * running, further results lost, so calls to next() will return results of
     * this query. The resultant Hashtable contains keys equal to the unbound
     * variables in the query, and values equal to their binding. If null is
     * returned, no bindings could satisfy the query ("no"). If there were no
     * unbound variables, an empty Hashtable is returned to signify "yes".
     * <p>
     * <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * 
     * @return a hashtable, containing the bindings generated.
     * @param query
     *                    a prolog query
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    an abnormal condition was detected
     */

    public Map query(String query) throws PrologException;

    public Map queryOnce(String query) throws PrologException;

    public List queryAll(String query) throws PrologException;

    /**
     * returns the next set of Bindings satisfying the last query.
     * 
     * <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * @return another set of Bindings
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    an IO Error occured .
     */

    public Map next() throws PrologException;

    /**
     * explicitly ends the last query, discarding further results if any
     * existed. If no query was active, this is a noop. <b>please note: </b>
     * <p>
     * It seems that interactive queries imply quiet a deal of extra complexity
     * in all the implementations i tried so far. They are also significantly
     * slower due to the added "synchronisation points" and the resulting io
     * overhead. At the moment i cannot think of any scenario that would require
     * the interactive concept anyway, so i would like to get rid of it asap.
     * 
     * @deprecated use queryAll or queryOnce instead
     * @throws IllegalStateException
     *                    the session is disposed
     * @throws PrologException
     *                    a lower-level failure has occured while killing the query.
     */

    public void endQuery() throws PrologException;

    /**
     * consults the File signified by the path passed into the Prolog System.
     * Typically implemented by a query. The return value is a "best effort"
     * guess, since several failed directives do not necessarly prompt Prolog to
     * consider the overall consultation failed. Consulted facts are (of course)
     * globally valid for all sessions.
     * 
     * @return true if the consultation seems to have succeeded, false if it
     *               definitly failed
     * @param name
     *                    a filename
     * @throws IllegalStateException
     *                    the session is disposed
     * @deprecated this method makes barely any sense at all.
     */

    public boolean consult(String name) throws PrologException;

    public void consult(String name, InputStream content)throws PrologException;   
//    public void unconsult(String name)throws PrologException;
//    public boolean isConsulted(String name)throws PrologException;


}
