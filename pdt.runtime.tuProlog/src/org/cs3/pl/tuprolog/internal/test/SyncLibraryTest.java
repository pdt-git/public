package org.cs3.pl.tuprolog.internal.test;

import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.tuprolog.internal.TuProlog;
import org.cs3.pl.tuprolog.internal.TuPrologPrologInterface;
import org.cs3.pl.tuprolog.internal.TuPrologSession;

import alice.tuprolog.Theory;

import junit.framework.TestCase;

public class SyncLibraryTest extends TestCase {
	private PrologInterface pif = null;


	
	protected void setUp() throws Exception {

		if (pif==null){
			pif = PrologInterfaceFactory.newInstance(TuProlog.FQN).create();
			pif.start();
			pif.setOption(TuPrologPrologInterface.WARNING, "true");
			
			((TuPrologPrologInterface)pif).getEngine().loadLibraryClass("org.cs3.pl.tuprolog.internal.SyncLibrary");
			
			Theory sync_test = new Theory(
											"sync(test).\n" +
											"add(X):- nonvar(X),assert(X).\n" +
											"remove(X):- nonvar(X),retract(X).");
			((TuPrologPrologInterface)pif).getEngine().addTheory(sync_test);
			
			Theory threads_test = new Theory(
					"counter(0).\n" +
					"counterInc :- counter(X), Addition is X+1, assert(counter(Addition)).\n" +
					"counterDec :- counter(X), Subtraction is X-1, assert(counter(Subtraction)).");
			((TuPrologPrologInterface)pif).getEngine().addTheory(threads_test);
		}
		
		super.setUp();
	}

	protected void tearDown() throws Exception {
		pif.stop();
	}

	public void testSync_0() throws Exception {
		
		PrologSession session = pif.getSession();
		
		assertNotNull(session.queryOnce("sync(test)."));
	}

	public void testWith_mutex_2() throws Exception {
		
		PrologSession session = pif.getSession();
		
		assertNotNull(session.queryOnce("with_mutex(ne,sync(X))."));		
	}
	
	Object monitor = new Object();
	int counter=0;
	
	public void testThreading() throws Exception {
	
		PrologSession session = pif.getSession();
		/*
		 * TODO create PrologInterface2 or wait till it get integerated
		 * 		in PrologInterface.
		 */
		
		/*
		  
		fail("Not fully Implemented");
		PrologEventDispatcher dispatcher = new PrologEventDispatcher((PrologInterface2) pif);
		
		
		PrologInterfaceListener locationListener = new PrologInterfaceListener(){
		public void update(PrologInterfaceEvent e) {
			System.out.println("LOCALISATION: RECEIVED EVENT: "+e.getEvent() + ", SUBJECT " + e.getSubject());
			System.out.flush();
			synchronized (monitor) {
				counter++;
				if(counter == 4) {
					monitor.notifyAll();
				}
			}
		}
		};
		
		//((TuPrologSession)session).setDispatcher(locationListener);
		dispatcher.addPrologInterfaceListener("with_mutex(Key,Goal)", locationListener );
		*/
		
		session.queryOnce("with_mutex(ne,add(test(9))).");
		assertNotNull("There should be a result return.",session.queryOnce("test(9)."));
		session.queryOnce("with_mutex(ne,remove(test(9))).");
		assertNull("There should be no result return.", session.queryOnce("test(9)."));
		session.queryOnce("with_mutex(ne,counterInc).");
		session.queryOnce("counter(X)");
		session.queryOnce("with_mutex(ne,counterDec).");
		
		session.dispose();
	}
	
	

}
