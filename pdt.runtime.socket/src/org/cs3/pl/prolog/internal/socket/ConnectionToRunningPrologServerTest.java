/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.prolog.internal.socket;

import junit.framework.TestCase;

import org.cs3.pl.prolog.PrologEventDispatcher;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;


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
		PrologEventDispatcher dispatcher = new PrologEventDispatcher((PrologInterface) pif);
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
		PrologEventDispatcher dispatcher = new PrologEventDispatcher((PrologInterface) pif);
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
		
		PrologInterface pif = SocketPrologInterface.newInstance();
		
		pif.setStandAloneServer(true);		
		pif.start();
		return pif;
	}
	
	public void testConnectionWorks() throws Throwable{
		pif.getSession().queryOnce("threads");
	}
}
