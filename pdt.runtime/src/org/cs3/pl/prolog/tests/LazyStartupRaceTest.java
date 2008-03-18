package org.cs3.pl.prolog.tests;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;

import junit.framework.TestCase;

public class LazyStartupRaceTest extends TestCase {
	 private PrologInterface pif;

	protected void setUp() throws Exception {
         Debug.setDebugLevel(Debug.LEVEL_DEBUG);
	     
	      pif=PrologInterfaceFactory.newInstance().create();
	      
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
