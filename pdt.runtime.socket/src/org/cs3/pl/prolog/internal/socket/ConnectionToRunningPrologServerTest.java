package org.cs3.pl.prolog.internal.socket;

import java.io.IOException;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;

public class ConnectionToRunningPrologServerTest extends TestCase {

	public final static String FACTORY="org.cs3.pl.prolog.internal.socket.Factory";

	static PrologInterface pif = null;

	protected void setUp() throws Exception {
		if(pif == null) {
			pif = init();
		}
		super.setUp();
	}
	
	protected void tearDown() throws Exception {
		
		super.tearDown();
	}
	
	Object monitor = new Object();
	int counter = 0;

	
	
	public void testConnect() throws Exception {

		PrologSession session = pif.getSession();
		
		PrologInterfaceListener listener = new PrologInterfaceListener(){
			public void update(PrologInterfaceEvent e) {
				System.out.println("RECEIVED EVENT: "+e.getEvent() + ", SUBJECT " + e.getSubject());
				System.out.flush();
				synchronized (monitor) {
					counter++;
					if(counter == 4) {
						monitor.notifyAll();
					}
				}
			}
		};

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
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(sync:init_term_ref))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(sync:notify_if_predicate_updated))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer, spy(erase))))");
		
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:handle_message))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:handle_command))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:notify/2))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:thread_get_message/1))))");
//		session.queryOnce("forall(current_thread(A,_),thread_signal(A,(guitracer,spy(consult_server:cleanup_thread/1))))");
		PrologEventDispatcher dispatcher = new PrologEventDispatcher((PrologInterface2) pif);
		dispatcher.addPrologInterfaceListener("localisation:company_nearby(MAC, Other, Distance, 1000)",locationListener);

		session.queryOnce("sync:deleteAll(magicmap:location('00-09-2D-53-27-3A', _,_,_))");
		//Samsung
		session.queryOnce("sync:add(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.12, 0))");
		

		session.queryOnce("sync:commit");
		synchronized (monitor) {
			monitor.wait(500);
		}			

		//IBM
		session.queryOnce("sync:deleteAll(magicmap:location('00-09-2D-53-27-3A', _,_,_))");
		session.queryOnce("sync:add(magicmap:location('00-09-2D-53-27-3A', 50.73, 7.122, 0))");
		session.queryOnce("sync:commit");
		synchronized (monitor) {
			monitor.wait(500);
		}			
		
		dispatcher.addPrologInterfaceListener("magicmap:position(A,B,C)",listener);
		
		try {
			session.queryOnce("sync:deleteAll(magicmap:position(_,_,_))");
			session.queryOnce("sync:add(magicmap:position(111,222,333))");
			session.queryOnce("sync:add(magicmap:position(222,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			session.queryOnce("sync:delete(magicmap:position(222,333,444))");
			session.queryOnce("sync:add(magicmap:position(222,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			session.queryOnce("sync:delete(magicmap:position(222,333,444))");
			session.queryOnce("sync:add(magicmap:position(242,333,444))");
			session.queryOnce("sync:commit");
			synchronized (monitor) {
				monitor.wait(500);
			}			
			System.out.flush();
			System.err.flush();
			System.out.flush();
			System.err.flush();
		} finally {
			dispatcher.stop();
			session.dispose();
		}
		synchronized (monitor) {
			monitor.wait(500);
		}
		assertEquals(counter, 4);
		
		
	}

	public void testErrors() throws Exception {
		PrologInterfaceListener nullListener = new PrologInterfaceListener(){
			public void update(PrologInterfaceEvent e) {
			}
		};
		PrologEventDispatcher dispatcher = new PrologEventDispatcher((PrologInterface2) pif);
		try {
			
			dispatcher.addPrologInterfaceListener("aha(",nullListener);
			fail("expected prolog exception");
		} catch(PrologException e) {

			String msg = e.getLocalizedMessage();
			assertTrue("unexpected error message: "+msg,msg.startsWith("Peer reported an error:error(syntax_error"));
		}finally{
			dispatcher.stop();
		}
	}


	private PrologInterface init() throws IOException {
		PrologInterfaceFactory factory= Factory.newInstance(FACTORY);
		PrologInterface pif = factory.create();
		pif.setOption(SocketPrologInterface.STANDALONE,"true");
		pif.start();
		return pif;
	}
	
	public void testConnectionWorks() throws Throwable{
		pif.getSession().queryOnce("threads");
	}
}
