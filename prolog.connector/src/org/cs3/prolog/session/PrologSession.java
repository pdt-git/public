/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.session;

import java.util.List;
import java.util.Map;

import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterfaceException;

public interface PrologSession extends Disposable{
	/**
	 * FIXME: Documentation 
	 * @param query
	 * @return null if query failed otherwise a map containing bindings for all variables
	 * @throws PrologException
	 * @throws PrologInterfaceException
	 */
    public Map<String,Object> queryOnce(String query) throws PrologException,PrologInterfaceException;
    public List<Map<String,Object>> queryAll(String query) throws PrologException,PrologInterfaceException;
}

