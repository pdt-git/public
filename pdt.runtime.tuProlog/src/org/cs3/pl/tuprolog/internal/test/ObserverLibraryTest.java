package org.cs3.pl.tuprolog.internal.test;

import org.cs3.pl.tuprolog.internal.ObservationListener;
import org.cs3.pl.tuprolog.internal.ObserverLibrary;
import org.cs3.pl.tuprolog.internal.SWICompatibilityLibrary;

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
			engine.loadLibrary(new SWICompatibilityLibrary());
			engine.loadLibrary(new ObserverLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testObserve() throws MalformedGoalException {
		SolveInfo info = engine.solve("observe(test, hi).");
		assertTrue(info.isSuccess());
	}
	
	public void testUnObserve() throws MalformedGoalException {
		SolveInfo info = engine.solve("observe(observe_test(_,Y), firstKey).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("observe(observe_test(_,X), firstKey).");
		assertFalse(info.isSuccess());
		
		info = engine.solve("unobserve(observe_test(_,_)).");
		assertTrue(info.isSuccess());
	}
	
	public void testNotify() throws MalformedGoalException, InterruptedException{
		SolveInfo info = engine.solve("observe(observe_test(_,Y), firstKey).");
		assertTrue(info.isSuccess());

		ObserverLibrary lib = (ObserverLibrary) engine.getLibrary("org.cs3.pl.tuprolog.internal.ObserverLibrary");
		ObservationListener ls = new ObservationListener(){

			public void onUpdate(String msg) {
				System.err.println(msg);				
			}
			
		};
		
		lib.addListener("observe_test(X,_)", ls );
		lib.addListener("observe_test(X,_)", ls );
		

		info = engine.solve("pif_notify(observe_test(_,_), 'first test for messages').");
		assertTrue(info.isSuccess());
		
		Thread.sleep(300);
		
		lib.removeAllListeners("observe_test(_,_)");
		info = engine.solve("pif_notify(observe_test(_,_), 'second test for messages').");
		assertTrue(info.isSuccess());
		
	}

}
