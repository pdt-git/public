package org.cs3.pl.tuprolog.internal.test;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
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
	private PrologSession session = null;


	
	protected void setUp() throws Exception {

		if (pif==null){
			pif = PrologInterfaceFactory.newInstance(TuProlog.FQN).create();
			pif.start();
			pif.setOption(TuPrologPrologInterface.WARNING, "true");
			
			((TuPrologPrologInterface)pif).getEngine().loadLibraryClass("org.cs3.pl.tuprolog.internal.SyncLibrary");
			
			Theory sync_test = new Theory(
											"sync(test).\n" +
											"add(X):- nonvar(X),assert(X).\n" +
											"remove(X):- nonvar(X),retract(X).\n" +
											"sleep(X) :- current_thread <- sleep(X).\n" +
											"thread_name(Name) :- var(Name), current_thread <- getName returns Name.");
			((TuPrologPrologInterface)pif).getEngine().addTheory(sync_test);
			session = pif.getSession();
		}
		
		super.setUp();
	}

	protected void tearDown() throws Exception {
		pif.stop();
	}

	public void testSync_0() throws Exception {
				
		assertNotNull(session.queryOnce("sync(test)."));
	}

	public void testWith_mutex_2() throws Exception {
				
		Map sync = session.queryOnce("with_mutex(ne,sync(X)).");
		assertEquals("X should be bind to test", "test", sync.get("X"));

		session.queryOnce("with_mutex(ne,add(jail(new_jail))).");
		sync = session.queryOnce("jail(X).");
		assertEquals("X should be bind to new_jail", "new_jail", sync.get("X"));
		
		session.queryOnce("with_mutex(ne,remove(jail(new_jail))).");
		sync = session.queryOnce("jail(X).");
		assertNull("Should be Null since there is no jail predicate.", sync);
				
	}
	
	Object monitor = new Object();
	int counter=0;
	
	public void testThreading() throws Exception {
			
		/*
		 * TODO create PrologInterface2 by creating AsyncSession or wait till it get integerated
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
		
		Thread first = new Thread("firstThread"){
			public void run() {
				try {
					/*
					System.err.println(getName()+" Adding first message");
					session.queryOnce("with_mutex('thread_test',add(first(message))).");
					
					System.err.println(getName()+" sleep : Started ");
					sleep(200);
					System.err.println(getName()+" sleep : ended ");
					
					System.err.println(getName()+" Query for second message");
					session.queryOnce("with_mutex('thread_test',second(message))");
					
					System.err.println(getName()+" Removing first message");
					session.queryOnce("with_mutex('thread_test',remove(first(message))).");
					*/
					System.err.println(getName()+" Started");
					Map info = session.queryOnce("with_mutex(test(threads),(assert(f(1)), sleep(5000), f(1))).");
					assertNotNull(info);
					info = session.queryOnce("with_mutex(second_test(threads),f(1)).");
					assertNotNull(info);
					System.err.println(getName()+" Ended");
					
				} catch (PrologException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (PrologInterfaceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} 
			}
		};

		Thread second = new Thread("secondThread"){
			public void run() {
				try {
					/*
					System.err.println(getName()+" Query for first message");
					session.queryOnce("with_mutex('thread_test',first(message)).");
					
					System.err.println(getName()+" Added second message");
					session.queryOnce("with_mutex('thread_test',add(second(message))).");
					
					System.err.println(getName()+" sleep : Started ");
					sleep(300);
					System.err.println(getName()+" sleep : ended ");
					
					System.err.println(getName()+" Query for first message");
					session.queryOnce("with_mutex('thread_test',first(message)).");
					*/
					System.err.println(getName()+" Started");
					Map info = session.queryOnce("with_mutex(test(threads),(assert(f(1)), retract(f(2)))).");
					assertNull(info);
					info = session.queryOnce("thread_name(Name).");
					assertEquals("secondThread", info.get("Name"));
					
					System.err.println(getName()+" Ended");
				} catch (PrologException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} catch (PrologInterfaceException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		};

		first.start();
		second.start();
		
		Thread.sleep(6000);
		session.dispose();
	}
	
	

}
