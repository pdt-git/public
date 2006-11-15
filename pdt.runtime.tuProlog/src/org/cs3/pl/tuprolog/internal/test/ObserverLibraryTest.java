package org.cs3.pl.tuprolog.internal.test;

import org.cs3.pl.tuprolog.internal.ObserverLibrary;

import junit.framework.TestCase;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;

public class ObserverLibraryTest extends TestCase {
	private Prolog engine = null ;

	protected void setUp() throws Exception {
		super.setUp();
		if ( engine == null ){
			engine = new Prolog();
			engine.loadLibrary(new ObserverLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testObserver() throws MalformedGoalException {
		SolveInfo info = engine.solve("observe('thread' , test, hi).");
		assertTrue(info.isSuccess());
	}

}
