package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.tuprolog.internal.ObserverLibrary;
import org.cs3.pl.tuprolog.internal.TuProlog;

import alice.tuprolog.InvalidTermException;
import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;

/*
 *  Note : To test ObserverLibrary, you need to load sync.pl first.
 */

public class ObserverLibraryTest extends TestCase {
	private TuProlog engine = null ;

	protected void setUp() throws Exception {
		super.setUp();
		if ( engine == null ){
			engine = new TuProlog();
			engine.initEngine();
			engine.loadLibrary("sync.pl");
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	public void testObserve() throws MalformedGoalException {
		engine.solve("assert(test).");
		SolveInfo info = engine.solve("observe(test).");
		assertTrue(info.isSuccess());
	}
	
	public void testUnObserve() throws MalformedGoalException {
		engine.solve("assert(observe_test(s)).");
		SolveInfo info = engine.solve("observe(observe_test(X)).");
		assertTrue(info.isSuccess());
		
		info = engine.solve("unobserve(observe_test(_)).");
		assertTrue(info.isSuccess());
	}
	
	private Object monitor = new Object();
	private int counter = 0;
	
	public void testNotify() throws MalformedGoalException, InterruptedException, InvalidTermException{
		
		engine.solve("assert(observe_test(s)).");

		SolveInfo info = engine.solve("observe(observe_test(X)).");
		assertTrue(info.isSuccess());
		ObserverLibrary lib = (ObserverLibrary) engine.getLibrary("ObserverLibrary");

		lib.addListener("observe_test(_)", new PrologInterfaceListener(){

			public void update(PrologInterfaceEvent e) {
				System.err.println(e.getSubject()+" "+e.getEvent());
				synchronized (monitor) {
					counter++;
					if (counter == 2)
						monitor.notifyAll();
				}
			}
			
		});
		
		info = engine.solve("pif_notify(observe_test(_), 'will be displayed .. 1').");
		assertTrue(info.isSuccess());

		info = engine.solve("pif_notify(observe_test(_), 'will be displayed .. 2').");
		assertTrue(info.isSuccess());
		
		synchronized (monitor) {
			monitor.wait(500);
		}
		
		lib.removeAllListeners("observe_test(_)");
		info = engine.solve("pif_notify(observe_test(_), 'will not be displayed .. 1').");
		assertTrue(info.isSuccess());
		
	}

}
