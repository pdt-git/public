package org.cs3.pl.prolog.internal;

import java.util.Map;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

/**
 * @author terra
 */
public class PrologSessionThrowTest extends TestCase {
	private PrologInterface pif;

    protected void setUp() throws Exception {
      pif=PrologInterfaceFactory.newInstance().create();
      pif.start();
    }
    
    protected void tearDown() throws Exception {
        pif.stop();
    }
	
	/**
	 * @see http://roots.iai.uni-bonn.de/jira/browse/PDT-10
	 * @throws PrologException
	 */
	public void testThrow() throws PrologException{
		PrologSession ss = pif.getSession();
		try  {
		Map query = ss.queryOnce("throw(A)");
		} catch(Exception ex){
			System.out.println("");
		}
		ss.dispose();
	}}
