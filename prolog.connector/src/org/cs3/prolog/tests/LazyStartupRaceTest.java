package org.cs3.prolog.tests;

import junit.framework.TestCase;

import org.cs3.prolog.PrologInterface;
import org.cs3.prolog.PrologInterfaceException;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.internal.AbstractPrologInterface;

public class LazyStartupRaceTest extends TestCase {
	 private PrologInterface pif;

	@Override
	protected void setUp() throws Exception {
         Debug.setDebugLevel(Debug.LEVEL_DEBUG);
	     
//	       pif=PrologInterfaceFactory.newInstance().create();
	      pif=AbstractPrologInterface.newInstance();
	      
	    }
	    
	    /* (non-Javadoc)
	     * @see junit.framework.TestCase#tearDown()
	     */
	    @Override
		protected void tearDown() throws Exception {
	        pif.stop();
	    }
	    
	    public void testLazyStartUp() throws PrologInterfaceException {
	    	pif.getSession();

		}
}
