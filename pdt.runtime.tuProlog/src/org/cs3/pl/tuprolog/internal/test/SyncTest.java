package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.tuprolog.internal.SWICompatibilityLibrary;
import org.cs3.pl.tuprolog.internal.TuProlog;

import alice.tuprolog.SolveInfo;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;

public class SyncTest extends TestCase {
	TuProlog engine = null;
	PrologSession session = null;

	protected void setUp() throws Exception {
		super.setUp();
		if ( engine == null){
			engine = new TuProlog();
			//engine.initEngine();
			
			engine.setSpy(true);
			engine.addSpyListener(new SpyListener(){

				public void onSpy(SpyEvent e) {
					// TODO Auto-generated method stub
					System.err.println("error"+e.getMsg());
				}
				
			});
			//Theory th = new Theory( );
			//engine.addTheory(th);
			engine.loadLibrary(new SWICompatibilityLibrary());
		}
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testSync() throws Exception {
		SolveInfo info = engine.solve("A=@=X.");
		
		assertTrue(info.isSuccess());
	}
}
