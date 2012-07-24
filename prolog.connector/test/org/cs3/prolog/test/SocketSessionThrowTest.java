/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

/**
 * @author terra
 */
public class SocketSessionThrowTest extends TestCase {
	private PrologInterface pif;

    @Override
	protected void setUp() throws Exception {
      pif = PrologRuntimePlugin.getDefault().newPrologInterface();
      pif.start();
    }
    
    @Override
	protected void tearDown() throws Exception {
        pif.stop();
    }
	
	/**
	 * @see http://roots.iai.uni-bonn.de/jira/browse/PDT-10
	 * @throws PrologException
	 * @throws PrologInterfaceException 
	 */
	public void testThrow() throws PrologException, PrologInterfaceException{
		PrologSession ss = pif.getSession();
		try  {
		ss.queryOnce("throw(A)");
		} catch(Exception ex){
			System.out.println("");
		}
		ss.dispose();
	}}

