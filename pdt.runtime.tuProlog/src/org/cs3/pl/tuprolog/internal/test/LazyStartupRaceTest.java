package org.cs3.pl.tuprolog.internal.test;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.tuprolog.internal.TuProlog;

import junit.framework.TestCase;

public class LazyStartupRaceTest extends TestCase {
	 private PrologInterface pif;

	protected void setUp() throws Exception {
         Debug.setDebugLevel(Debug.LEVEL_DEBUG);
	     
	      pif=PrologInterfaceFactory.newInstance(TuProlog.FQN).create();
	      
	    }
	    
	    /* (non-Javadoc)
	     * @see junit.framework.TestCase#tearDown()
	     */
	    protected void tearDown() throws Exception {
	        pif.stop();
	    }
	    
	    public void testLazyStartUp() throws PrologInterfaceException {
	    	pif.getSession();

		}
}
