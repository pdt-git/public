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

package org.cs3.pl.prolog.tests;

import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CNil;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.AsyncPrologSessionEvent;
import org.cs3.pl.prolog.AsyncPrologSessionListener;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public class AsyncSocketSessionTest extends TestCase {

	private PrologInterface pif;

	private Recorder rec;

	private AsyncPrologSession session;

	protected void setUp() throws Exception {
		Debug.setDebugLevel(Debug.LEVEL_DEBUG);
//		PrologInterfaceFactory factory = PrologInterfaceFactory.newInstance();
//		pif = (PrologInterface) factory.create();
		pif = AbstractPrologInterface.newInstance();

		pif.start();
		rec = new Recorder();
		session = pif.getAsyncSession();
		session.addBatchListener(rec);
	}

	protected void tearDown() throws Exception {
		pif.stop();
	}

	class Record {
		String method;

		AsyncPrologSessionEvent event;

		public Record(String method, AsyncPrologSessionEvent event) {
			this.method = method;
			this.event = event;

		}

		public boolean isSyntaxError() {
			// error(syntax_error(cannot_start_term), stream($stream(_), 9, 0,
			// 116))
			CTerm msg = PLUtil.createCTerm(event.message);
			if (!(msg instanceof CCompound)) {
				return false;
			}
			CCompound c = (CCompound) msg;
			if (c.getArity() != 2 || !c.getFunctorValue().equals("error")) {
				return false;
			}
			CTerm arg = c.getArgument(0);
			if (arg.getArity() != 1
					|| !arg.getFunctorValue().equals("syntax_error")) {
				return false;
			}
			return true;
		}

		public String toString() {
			StringBuffer sb = new StringBuffer();
			sb.append(method);
			sb.append('(');
			if (event.ticket instanceof String) {
				sb.append(event.ticket == null ? "null" : event.ticket
						.toString());
			} else {
				sb.append(event.ticket == null ? "null" : "dummy");
			}
			sb.append(',');
			sb.append(event.message == null ? "null" : Util.hideStreamHandles(
					event.message, "$stream(_)"));
			sb.append(',');
			sb.append(event.bindings == null ? "null" : "("
					+ Util.prettyPrint(event.bindings) + ")");
			sb.append(')');
			return sb.toString();
		}
	}

	class Recorder implements AsyncPrologSessionListener {
		public void clear() {
			records.clear();
		}

		public synchronized Record last() {
			return (Record) records.lastElement();
		}

		public Record get(int i) {
			return records.get(i);
		}

		public String toString() {
			StringBuffer sb = new StringBuffer();
			boolean first = true;
			for (Iterator it = records.iterator(); it.hasNext();) {
				Record r = (Record) it.next();
				if (!first) {
					sb.append(", ");
				}
				sb.append(r.toString());
				first = false;
			}
			return sb.toString();
		}

		Vector<Record> records = new Vector<Record>();

		public synchronized void joinComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("joinComplete", e));
			notifyAll();
		}

		public synchronized void abortComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("abortComplete", e));
			notifyAll();
		}

		public synchronized void goalSucceeded(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSucceeded", e));
			notifyAll();
		}

		public synchronized void goalFailed(AsyncPrologSessionEvent e) {
			records.add(new Record("goalFailed", e));
			notifyAll();
		}

		public synchronized void goalRaisedException(AsyncPrologSessionEvent e) {
			records.add(new Record("goalRaisedException", e));
			notifyAll();
		}

		public synchronized void goalHasSolution(AsyncPrologSessionEvent e) {
			records.add(new Record("goalHasSolution", e));
			notifyAll();
		}

		public synchronized void goalSkipped(AsyncPrologSessionEvent e) {
			records.add(new Record("goalSkipped", e));
			notifyAll();
		}

		public synchronized void goalCut(AsyncPrologSessionEvent e) {
			records.add(new Record("goalCut", e));
			notifyAll();
		}

		public synchronized void batchComplete(AsyncPrologSessionEvent e) {
			records.add(new Record("batchComplete", e));
			notifyAll();
		}

		public int size() {

			return records.size();
		}

	}

	public void test_syntaxError() throws Throwable {
		session.queryOnce("3", "member(a,[a,b,c)");
		session.dispose();
		assertTrue(rec.get(0).isSyntaxError());
	}

	public void test_syntaxError2() throws Throwable {
		session.queryOnce("1", "member(a,[a,b,c)");
		session.queryOnce("2", "member(a,[a,b,c)");
		session.dispose();
		assertEquals(3, rec.size());
		assertTrue(rec.get(0).isSyntaxError());
	}

	public void test_queryOnce_failure01() throws Throwable {
		session.queryOnce("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalFailed(4,null,null)", rec.get(0).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(1).toString());
		assertEquals(2, rec.size());
	}

	public void test_queryOnce_sequence01() throws Throwable {
		// PrologSession session=pif.getSession();
		session.queryOnce("1", "member(A,[a,b,c])");
		session.queryOnce("2", "member(a,[a,b,c])");
		session.queryOnce("3", "member(a,[a,b,c)");
		session.queryOnce("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a))", rec.get(0).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(1).toString());
		assertEquals("goalHasSolution(2,null,())", rec.get(2).toString());
		assertEquals("goalSucceeded(2,null,null)", rec.get(3).toString());
		assertTrue(rec.get(4).isSyntaxError());
		assertEquals("goalFailed(4,null,null)", rec.get(5).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(6).toString());
	}

	public void test_queryAll_sequence01() throws Throwable {
		// PrologSession session=pif.getSession();
		session.queryAll("1", "member(A,[a,b,c])");
		session.queryAll("2", "member(a,[a,b,c])");
		session.queryAll("3", "member(a,[a,b,c)");
		session.queryAll("4", "member(aA,[a,b,c])");
		session.dispose();
		assertEquals("goalHasSolution(1,null,(A-->a))", rec.get(0).toString());
		assertEquals("goalHasSolution(1,null,(A-->b))", rec.get(1).toString());
		assertEquals("goalHasSolution(1,null,(A-->c))", rec.get(2).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(3).toString());
		assertEquals("goalHasSolution(2,null,())", rec.get(4).toString());
		assertEquals("goalSucceeded(2,null,null)", rec.get(5).toString());

		assertTrue(rec.get(6).isSyntaxError());

		assertEquals("goalFailed(4,null,null)", rec.get(7).toString());
		assertEquals("batchComplete(null,null,null)", rec.get(8).toString());
	}

	public void testList() throws Throwable {
		session.queryOnce("1", "A=[0,1,2]");
		session.join();
		Record last = rec.get(0);
		AsyncPrologSessionEvent event = last.event;
		Map bindings = event.bindings;
		Object object = bindings.get("A");
		assertNotNull(object);
		assertTrue(object instanceof List);
		List l = (List) object;
		assertEquals(3, l.size());
		for (int i = 0; i < 3; i++) {
			Object o = l.get(i);
			assertEquals(("" + i), o);

		}
	}

	public void testPDT287_0() throws Exception {
		

		// atoms should be quoted.
		session.queryOnce("quoted", "atom_codes(A,[123,10])",PrologInterface.NONE);

		// terms should be canonical.
		session.queryOnce("canonical", "A=('B'+'C')",PrologInterface.NONE);

		// lists should be ignored
		session.queryOnce("ignore_lists", "A=[1,2]",PrologInterface.NONE);

		session.join();
		assertEquals("'{\\n'", rec.get(0).event.bindings.get("A"));
		assertEquals("+('B', 'C')", rec.get(2).event.bindings.get("A"));
		assertEquals("'.'(1, '.'(2, []))", rec.get(4).event.bindings.get("A"));

	}

	public void testPDT287_1() throws Exception {
		
		session.queryOnce("unquoted", "atom_codes(A,[123,10])",PrologInterface.UNQUOTE_ATOMS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologInterface.UNQUOTE_ATOMS);
		session.queryOnce("ignore_lists", "A=[1,2]",PrologInterface.UNQUOTE_ATOMS);

		session.join();
		// atoms should be unquoted.
		assertEquals("{\n", rec.get(0).event.bindings.get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.bindings.get("A"));

		// lists should be ignored
		assertEquals("'.'(1, '.'(2, []))", rec.get(4).event.bindings.get("A"));

	}

	public void testPDT291_nil() throws Exception{
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologInterface.PROCESS_LISTS);
		session.join();		
		assertTrue(rec.get(0).event.bindings.get("A") instanceof List);
		
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologInterface.NONE);
		session.join();
		assertTrue(rec.get(0).event.bindings.get("A") instanceof String);
		
		rec.clear();
		session.queryOnce("nabla", "A=[]", PrologInterface.CTERMS);
		session.join();
		assertTrue(rec.get(0).event.bindings.get("A") instanceof CNil);
	}
	
	public void testPDT287_2() throws Exception {
		
		session.queryOnce("quoted", "atom_codes(A,[123,10])",PrologInterface.PROCESS_LISTS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologInterface.PROCESS_LISTS);
		session.queryOnce("process_lists", "A=[[1,2],'A']",PrologInterface.PROCESS_LISTS);
		session.join();
		
		// atoms should be quoted.
		assertEquals("'{\\n'", rec.get(0).event.bindings.get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.bindings.get("A"));

		// lists should be processed
		Object o = rec.get(4).event.bindings.get("A");
		assertTrue(o instanceof List);

		// list elements should be processed recursively
		List l = (List) o;
		assertTrue(l.get(0) instanceof List);
		assertEquals("'A'", l.get(1));

	}

	public void testPDT287_3() throws Exception {
		
		session.queryOnce("unquoted", "atom_codes(A,[123,10])",PrologInterface.PROCESS_LISTS
				| PrologInterface.UNQUOTE_ATOMS);
		session.queryOnce("canonical", "A=('B'+'C')",PrologInterface.PROCESS_LISTS
				| PrologInterface.UNQUOTE_ATOMS);
		session.queryOnce("process_lists", "A=[[1,2],'A']",PrologInterface.PROCESS_LISTS
				| PrologInterface.UNQUOTE_ATOMS);
		session.join();
		// atoms should be unquoted.
		assertEquals("{\n", rec.get(0).event.bindings.get("A"));

		// terms should be canonical.
		assertEquals("+('B', 'C')", rec.get(2).event.bindings.get("A"));

		// lists should be processed
		Object o = rec.get(4).event.bindings.get("A");
		assertTrue(o instanceof List);

		// list elements should be processed recursively
		List l = (List) o;
		assertTrue(l.get(0) instanceof List);
		assertEquals("A", l.get(1));

	}

	public void testPDT287_4() throws Exception {
		
		// Everything should be CTerms, no list Processing.
		session.queryOnce("bla", "A=[[1,2],'A']",PrologInterface.CTERMS);
		session.join();
		Object o = rec.get(0).event.bindings.get("A");
		assertTrue(o instanceof CCompound);

	}

	public void testPDT287_illegal_session() throws Exception {
		// combination of CTERMS and UNQUOTE_ATOMS is illegal.
		try {
			AsyncPrologSession session = pif.getAsyncSession(PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			AsyncPrologSession session = pif.getAsyncSession(PrologInterface.CTERMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			AsyncPrologSession session = pif.getAsyncSession(PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
	
	public void testPDT287_illegal_query() throws Exception {
		// combination of CTERMS and UNQUOTE_ATOMS is illegal.
		try {
			session.queryOnce("bla","syntax error", PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			session.queryOnce("bla","syntax error", PrologInterface.CTERMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			session.queryOnce("bla","syntax error", PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
	
	public void test_longAtom() throws Throwable {
		// session.queryOnce("0", "guitracer");
		// session.queryOnce("0", "trace");
		// session.join();
		// rec.clear();
		StringBuffer sb = new StringBuffer();
		sb.append("atom(");
		for (int i = 0; i < 600000; i++) {
			sb.append('a');
		}
		sb.append(")");

		session.queryOnce("1", sb.toString());
		session.join();
		assertEquals("goalHasSolution(1,null,())", rec.get(0).toString());
		assertEquals("goalSucceeded(1,null,null)", rec.get(1).toString());
		assertEquals("joinComplete(dummy,null,null)", rec.get(2).toString());

	}

	public void test_pending_during_queryall() throws Throwable {
		String alias = session.getProcessorThreadAlias();
		String ticket = "queryall";
		session
				.queryAll(
						ticket,
						"repeat,writeln(waiting),thread_get_message(test(M)),writeln(got(M)),(M==stop,!;true)");
		PrologSession syncSession = pif.getSession();
		rec.clear();
		synchronized (rec) {
			syncSession.queryOnce("writeln('i am here'),thread_send_message('"
					+ alias + "',test(1))");
			rec.wait();
		}
		boolean pending1 = session.isPending(ticket);
		synchronized (rec) {
			syncSession.queryOnce("thread_send_message('" + alias
					+ "',test(stop))");
			rec.wait();
		}
		session.join();
		boolean pending2 = session.isPending(ticket);
		assertEquals("not pending during queryall", true, pending1);
		assertEquals("pending after queryall", false, pending2);
	}

	/*
	 * have one query with a blocking io call. queue a second one, which serves
	 * as a dummy. abort the batch. the processor cannot recieve the async abort
	 * request while the first query blocks. unlock the first query. the
	 * processor should now recieve the async abort request, it should skip the
	 * second goal.
	 * 
	 * Note: with the original implementation, cuts where reported after
	 * solutions, so the the first solution would be reported by the session.
	 * The current pifcom implementation checks for cut events first, so first
	 * solution is NOT reported. I figured that this change in semantics would
	 * not be of much relevants in practical applications, and it is much easier
	 * to realize on the server side.
	 */
	public void test_abort01() throws Throwable {
		session.queryOnce("1", "thread_self(Alias)");
		session.join();
		Record r = (Record) rec.records.get(0);
		final String alias = (String) r.event.bindings.get("Alias");
		// session.queryOnce("debug", "guitracer");
		// session.queryOnce("debug", "spy(handle_batch_command)");
		session.queryAll("2", "repeat,thread_get_message(test(M))");
		final PrologSession syncSession = pif.getSession();

		synchronized (rec) {
			syncSession.queryOnce("thread_send_message('" + alias
					+ "',test(1))");
			rec.wait();
		}

		rec.clear();
		assertFalse(session.isIdle());
		session.queryOnce("3", "should_be_skipped");
		final Object lock = new Object();

		synchronized (lock) {
			Thread thread = new Thread() {
				public void run() {

					// we need to make sure that test(2) (see below) is send
					// AFTER
					// the abort call, otherwise, abort will lock up forever.
					Debug.debug("enter 3");
					synchronized (lock) {
						Debug.debug("enter 5");
						try {

							Debug.debug("enter 6");
							syncSession.queryOnce("thread_send_message('"
									+ alias + "',test(2))");
						} catch (PrologException e) {
							Debug.report(e);

						} catch (PrologInterfaceException e) {
							Debug.report(e);
						}
					}

				}
			};
			Debug.debug("enter 0");
			thread.start();
			Debug.debug("enter 1");
			session.abort(lock);
			Debug.debug("enter 7");
		}
		// session.queryOnce("toggle uitracer","guitracer");
		// session.queryOnce("start tracer","trace");

		session.dispose();
		assertEquals(/*
						 * "goalHasSolution(2,null,(M-->2)), " +
						 */"goalCut(2,null,null), " + "goalSkipped(3,null,null), "
				+ "abortComplete(dummy,null,null), "
				+ "batchComplete(null,null,null)", rec.toString());

	}

	public void test_abort02() throws Exception {
		session.abort();
		session.dispose();

	}

	

	public void test_manyAsyncSessions() throws Throwable {
		int N = 30;
		AsyncPrologSession[] sessions = new AsyncPrologSession[N];
		for (int i = 0; i < N; i++) {
			sessions[i] = pif.getAsyncSession();
		}
		try {
			AsyncPrologSession session = pif.getAsyncSession();

		} catch (PrologInterfaceException e) {
			e.printStackTrace();
		}
		for (int i = 0; i < N; i++) {
			sessions[i].dispose();
		}
	}
}
