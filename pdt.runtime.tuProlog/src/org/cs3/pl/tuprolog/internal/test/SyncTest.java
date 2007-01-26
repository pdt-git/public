package org.cs3.pl.tuprolog.internal.test;

import java.util.Map;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.tuprolog.internal.TuProlog;
import org.cs3.pl.tuprolog.internal.TuPrologEventDispatcher;
import org.cs3.pl.tuprolog.internal.TuPrologPrologInterface;

import alice.tuprolog.event.OutputEvent;
import alice.tuprolog.event.OutputListener;
import alice.tuprolog.event.SpyEvent;
import alice.tuprolog.event.SpyListener;

public class SyncTest extends TestCase {
	//TuProlog engine = null;
	//PrologSession session = null;
	PrologInterface  pif = null;

	protected void setUp() throws Exception {
		super.setUp();
		
		
		if (pif==null){
			pif=PrologInterfaceFactory.newInstance(TuProlog.FQN).create();
			pif.start();
		}

	}

	protected void tearDown() throws Exception {
		super.tearDown();
		pif.stop();
	}

	private Object monitor = new Object();
	private int counter = 0;
	
	private void loadLibraries() throws Exception{
		TuProlog engine = ((TuPrologPrologInterface)pif ).getEngine();
		
		engine.loadLibrary("sync.pl");
		engine.loadLibrary("magicmap.pl");
		engine.loadLibrary("localisation.pl");		
		
	}
	
	public void testSyncModules() throws Exception{
		//TODO Still shows some warning of redefinition of ':' predicate while asserting facts into the database.
		loadLibraries();
		
		PrologSession session = pif.getSession();

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
		
		TuPrologEventDispatcher dispatcher = new TuPrologEventDispatcher((TuPrologPrologInterface) pif);
		dispatcher.addPrologInterfaceListener("company_nearby(MAC, Other, Distance, 1000)",locationListener);

		((TuPrologPrologInterface)pif).getEngine().addOutputListener(new OutputListener(){

			public void onOutput(OutputEvent e) {
				// TODO Auto-generated method stub
//				System.out.print(e.getMsg());
			}
			
		});
		
		((TuPrologPrologInterface)pif).getEngine().addSpyListener(new SpyListener(){

			public void onSpy(SpyEvent e) {
				// TODO Auto-generated method stub
				System.err.println(e.getMsg());
			}
			
		});
		
		//((TuPrologPrologInterface)pif).getEngine().setSpy(true);
		
		session.queryOnce("sync:deleteAll(location('00-09-2D-53-27-3A', _,_,_))");
		//Samsung
		session.queryOnce("sync:add(location('00-09-2D-53-27-3A', 50.73, 7.12, 0))");
		session.queryOnce("sync:commit");

		synchronized (monitor) {
			monitor.wait(500);
		}			

		//IBM
		session.queryOnce("sync:deleteAll(location('00-09-2D-53-27-3A', _,_,_))");
		session.queryOnce("sync:add(location('00-09-2D-53-27-3A', 50.73, 7.122, 0))");
		session.queryOnce("sync:commit");
		synchronized (monitor) {
			monitor.wait(500);
		}	
		
		session.dispose();
	}
	
	public void testSyncNoModules() throws Exception {
		
		loadLibraries();
		
		((TuPrologPrologInterface)pif).getEngine().addSpyListener(new SpyListener(){

			public void onSpy(SpyEvent e) {
				// TODO Auto-generated method stub
				System.out.println(e.getMsg());
			}
			
		});

		((TuPrologPrologInterface)pif).getEngine().addOutputListener(new OutputListener(){

			public void onOutput(OutputEvent e) {
				// TODO Auto-generated method stub
//				System.out.print(e.getMsg());
			}
			
		});
//		((TuPrologPrologInterface)pif).getEngine().setSpy(true);		
		
		
		PrologSession session = pif.getSession();

		session.queryOnce("sync:add(observation(d)).");
		session.queryOnce("sync:commit.");

		PrologInterfaceListener obs_listener = new PrologInterfaceListener(){

			public void update(PrologInterfaceEvent e) {
				System.out.println("OBSERVATION: RECEIVED EVENT: "+e.getEvent() + ", SUBJECT " + e.getSubject());
				System.out.flush();
				synchronized (monitor) {
					counter++;
					if(counter == 4) {
						monitor.notifyAll();
					}
				}
			}
			
		};
		

		
		TuPrologEventDispatcher dispatcher = new TuPrologEventDispatcher((TuPrologPrologInterface) pif);
		dispatcher.addPrologInterfaceListener("observation(_)", obs_listener);

		session.queryOnce("sync:add(observation(d)).");
		session.queryOnce("sync:add(observation(s)).");
		session.queryOnce("sync:commit.");
		

		session.queryOnce("sync:add(observation(e)).");
		session.queryOnce("sync:add(observation(o)).");
		session.queryOnce("sync:commit.");
		
		session.queryOnce("sync:add(observation(x)).");
		session.queryOnce("sync:add(observation(y)).");
		session.queryOnce("sync:commit.");
		
		session.queryOnce("sync:add(observation(r)).");
		session.queryOnce("sync:add(observation(z)).");
		session.queryOnce("sync:commit.");


		synchronized (monitor) {
			monitor.wait(500);			
		}
		
		assertEquals(4, counter);
		
		session.dispose();
	}
	
	
	public void testMultipleSessions() throws Exception {
		
		loadLibraries();
	
		PrologSession s1 = pif.getSession();
		PrologSession s2 = pif.getSession();
		PrologSession s3 = pif.getSession();
		
		s1.queryOnce("sync:add(session1(a))");
		s1.queryOnce("sync:add(session1(b))");
		s1.queryOnce("sync:add(session1(c))");
		s1.queryOnce("sync:commit");
		
		s2.queryOnce("sync:add(session2(a))");
		s2.queryOnce("sync:add(session2(b))");
		s2.queryOnce("sync:add(session2(c))");
		s2.queryOnce("sync:rollback");
		
		s3.queryOnce("sync:add(session3(a))");
		s3.queryOnce("sync:add(session3(b))");
		s3.queryOnce("sync:add(session3(c))");
		s3.queryOnce("sync:commit");
		
		assertNotNull("session 3 facts are expected to be in the factbase.", s1.queryOnce("session3(a)"));
		assertNotNull("session 1 facts are expected to be in the factbase.", s1.queryOnce("session1(a)"));
		assertNull("session 2 facts are not expected to be in the factbase.", s1.queryOnce("session2(a)"));
		
		s1.dispose();
		s2.dispose();
		s3.dispose();
	}

}
