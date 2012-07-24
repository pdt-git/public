/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.prolog.connector.ui.test;

import junit.framework.TestCase;

import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.lifecycle.PrologEventDispatcher;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceEvent;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.PrologInterfaceListener;
import org.cs3.prolog.session.PrologSession;


/**
 * 
 * Before running this test, please start an external prolog process and consult the file
 * main_socket.pl 
 * TODO: include this in the test fixture.
 *
 */
public class ConnectionToRunningPrologServerTest extends TestCase {

	public final static String FACTORY="org.cs3.pl.prolog.internal.socket.Factory";

	static PrologInterface pif = null;

	@Override
	protected void setUp() throws Exception {
		if(pif == null) {
			pif = init();
		}
		super.setUp();
	}
	
	@Override
	protected void tearDown() throws Exception {
		
		super.tearDown();
	}
	
	Object monitor = new Object();
	int counter = 0;

	
	
	public void testConnect() throws Exception {

		PrologSession session = pif.getSession(PrologInterface.LEGACY);
		
		PrologInterfaceListener listener = new PrologInterfaceListener(){
			@Override
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
			@Override
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
		PrologEventDispatcher dispatcher = new PrologEventDispatcher(pif,PrologRuntimeUIPlugin.getDefault().getLibraryManager());
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
			@Override
			public void update(PrologInterfaceEvent e) {
			}
		};
		PrologEventDispatcher dispatcher = new PrologEventDispatcher(pif,PrologRuntimeUIPlugin.getDefault().getLibraryManager());
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


	private PrologInterface init() throws PrologInterfaceException {
//		PrologInterfaceFactory factory= Factory.newInstance(FACTORY);
//		PrologInterface pif = factory.create();
		
        pif = PrologRuntimePlugin.getDefault().newPrologInterface();
		pif.setStandAloneServer(true);		
		pif.start();
		return pif;
	}
	
	public void testConnectionWorks() throws Throwable{
		pif.getSession(PrologInterface.LEGACY).queryOnce("threads");
	}
}


