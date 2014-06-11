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

package org.cs3.prolog.test;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import junit.framework.TestCase;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.cterm.CCompound;
import org.cs3.prolog.connector.cterm.CNil;
import org.cs3.prolog.connector.internal.process.AbstractPrologInterface;
import org.cs3.prolog.connector.process.PrologException;
import org.cs3.prolog.connector.process.PrologInterface;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.cs3.prolog.connector.session.PrologSession;

/**
 * @author Lukas Degener
 * 
 * 
 */
public class SocketSessionTest extends TestCase {
	private PrologInterface pif;

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		Debug.setDebugLevel("DEBUG");
		pif = Connector.newPrologInterface();
		pif.start();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		pif.stop();
	}

	public void testCreation() throws PrologInterfaceException {
		PrologSession ss = pif.getSession();

		assertNotNull(ss);
		ss.dispose();
	}

	public void testRepeatedStartup() throws Throwable {

		class _Thread extends Thread {
			Exception e = null;

			@Override
			public void run() {
				for (int i = 0; i < 5; i++) {
					try {
						pif.stop();
						pif.start();
					} catch (PrologInterfaceException e) {
						this.e = e;
					}

				}
			}
		}
		;
		_Thread t = new _Thread();
		t.start();
		for (int i = 0; i < 5; i++) {
			pif.stop();
			pif.start();
		}
		t.join();
		assertNull(t.e);
	}

	public void testQueries() throws PrologException, PrologInterfaceException {
		PrologSession ss = pif.getSession();

		assertNotNull(ss.queryOnce("assert(a(b))"));
		assertNotNull(ss.queryOnce("assert(a(v))"));
		assertNotNull(ss.queryOnce("a(v)"));
		assertNull(ss.queryOnce("a(j)"));

		List<Map<String, Object>> tabs = ss.queryAll("a(X)");

		assertEquals(2, tabs.size());
		assertTrue(tabs.get(0).containsKey("X"));
		assertTrue(tabs.get(1).containsKey("X"));

		assertEquals("b", tabs.get(0).get("X"));
		assertEquals("v", tabs.get(1).get("X"));

		ss.dispose();
	}

	public void testCTerm() throws PrologException, PrologInterfaceException {
		PrologSession s = pif.getSession(PrologInterface.CTERMS);		
		Map<String,Object> map = s.queryOnce("A=[1,2]");
		assertNotNull(map);
		Object a = map.get("A");
		assertTrue(a instanceof CCompound);

	}

	public void testListProcessingDisabled() throws PrologException,
			PrologInterfaceException {
		PrologSession s =  pif.getSession(PrologInterface.NONE);
		
		Map<String,Object> map = s.queryOnce("A=[1,2]");
		assertNotNull(map);
		Object a = map.get("A");
		assertTrue(a instanceof String);
		String actual = (String) a;
		assertEquals("'.'(1, '.'(2, []))", actual);

	}

		

	public void testDispose() throws PrologException, PrologInterfaceException {
		PrologSession ss = pif.getSession();

		ss.dispose();

		try {
			ss.queryOnce("a(X).");
		} catch (IllegalStateException e) {
			return;
		}

		fail("Exception thrown on disposed object");
	}

	public void testMultipleQuery() throws PrologException,
			PrologInterfaceException {
		PrologSession server = pif.getSession();

		// ClientConnection connection= new ClientConnectionStub();
		Map<String,Object> r = server.queryOnce("assert(wahr(wahrheit))");
		assertNotNull("result should not be null", r);
		assertTrue("result should be empty", r.isEmpty());

		r = server.queryOnce("assert(wahr(wahr(wahrheit)))");
		assertNotNull("result should not be null", r);
		assertTrue("result should be empty", r.isEmpty());

		r = server.queryOnce("wahr(A)");
		assertNotNull("result should not be null", r);
		assertTrue("result should be not empty", !r.isEmpty());

		List<Map<String, Object>> v = server.queryAll("wahr(A)");

		assertEquals("there should be exactly two solutions!", 2, v.size());
		assertEquals("wahrheit", v.get(0).get("A"));
		assertEquals("wahr(wahrheit)", v.get(1).get("A"));

	}

	/**
	 * PDT-205
	 * 
	 * @throws PrologInterfaceException
	 */
	public void testSyntaxError() throws PrologInterfaceException {
		PrologSession s = pif.getSession();
		try {
			s.queryAll("test(''asdf'')");
		} catch (PrologException e) {
			;
		}
	}

	public void testIOProblem() throws Throwable {
		final PrologSession s = pif.getSession();
		final Throwable[] t = new Throwable[1];
		Thread thread = new Thread() {

			@Override
			public void run() {
				try {
					s.queryAll("thread_get_message(knarst)");
					t[0] = null;
				} catch (PrologException e) {
					t[0] = e;
				} catch (PrologInterfaceException e) {
					t[0] = e;
				}
			}
		};
		thread.start();

		((AbstractPrologInterface) pif).getStartAndStopStrategy().stopServer(
				pif);
		thread.join();
		assertNotNull(t[0]);
		assertEquals(PrologInterfaceException.class, t[0].getClass());
		pif.restart();
	}

	/**
	 * this one fails if the pif impl does not support lists
	 * 
	 * @throws PrologInterfaceException
	 */
	public void testList() throws PrologInterfaceException {
		PrologSession s = pif.getSession();
		Map<String,Object> map = s.queryOnce("A=[1,2,3,[[a,b,['{}']]],[b,c]]");

		Object A = map.get("A");
		assertEquals("[1, 2, 3, [[a, b, [{}]]], [b, c]]", A.toString());
		assertTrue(A instanceof List<?>);
		List<?> l = (List<?>) A;
		assertEquals("1", (String) l.get(0));
		assertEquals("2", (String) l.get(1));
		assertEquals("3", (String) l.get(2));
		assertEquals(5, l.size());
		A = l.get(3);
		assertTrue(A instanceof List<?>);
		List<?> m = (List<?>) A;
		assertEquals(1, m.size());
		assertTrue(m.get(0) instanceof List<?>);
		m = (List<?>) m.get(0);
		assertEquals(3, m.size());
		assertEquals("a", (String) m.get(0));
		assertEquals("b", (String) m.get(1));
		assertTrue(m.get(2) instanceof List<?>);
		m = (List<?>) m.get(2);
		assertEquals(1, m.size());
		assertEquals("{}", (String) m.get(0));
		assertTrue(l.get(4) instanceof List<?>);
		m = (List<?>) l.get(4);
		assertEquals(2, m.size());
		assertEquals("b", (String) m.get(0));
		assertEquals("c", (String) m.get(1));

	}

	public void testQueryAll() throws Throwable {
		PrologSession s = pif.getSession();

		List<Map<String,Object>> result = s.queryAll("member(A,[ich,du,muellers_kuh])");
		assertEquals(3, result.size());
		Vector<Object> v = new Vector<Object>();
		for (Iterator<Map<String,Object>> it = result.iterator(); it.hasNext();) {
			Map<String,Object> m = it.next();
			v.add(m.get("A"));
		}
		assertEquals("ich", v.get(0));
		assertEquals("du", v.get(1));
		assertEquals("muellers_kuh", v.get(2));

		result = s.queryAll("member(ich,[ich,ich,muellers_kuh])");
		assertEquals(2, result.size());
		for (Iterator<Map<String,Object>> it = result.iterator(); it.hasNext();) {
			Map<String,Object> m = it.next();
			assertEquals(0, m.size());
		}
		result = s.queryAll("member(und,[ich,du,muellers_kuh])");
		assertEquals(0, result.size());

	}

	public void _test_longAtomLength() throws Throwable {
		StringBuffer sb = new StringBuffer();

		int len = 600000;
		for (int i = 0; i < len; i++) {
			sb.append('a');
		}

		String atom = sb.toString();
		PrologSession s = pif.getSession();
		Map<String,Object> map = s.queryOnce("atom_length(" + atom + ",1,3,_,Sub)");
		assertEquals("aaa", map.get("Sub"));
	}

	public void _test_longAtom_result() throws Throwable {
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < 600000; i++) {
			sb.append('a');
		}

		String atom = sb.toString();
		PrologSession s = pif.getSession();
		Map<String,Object> map = s.queryOnce("A=" + atom);
		assertEquals(atom, map.get("A"));
	}

	public void test_longAtom() throws Throwable {
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < 600000; i++) {
			sb.append('a');
		}

		String atom = sb.toString();
		PrologSession s = pif.getSession();
		assertNotNull(s.queryOnce("atom(" + atom + ")"));

	}

	/**
	 * PDT-195
	 */
	public void testTurkish() throws Exception {
		PrologSession session = pif.getSession();
		String query = "A=kad\u0131n(nurhan)";
		Map<String,Object> map = session.queryOnce(query);
		String actual = (String) map.get("A");
		assertEquals("kad\u0131n(nurhan)", actual);
	}

	public void testTurkishX() throws Exception {
		String expected = "kad\u0131n(nurhan)";
		File file = File.createTempFile("testturkish", ".txt");
		PrintWriter out = new PrintWriter(new OutputStreamWriter(
				new FileOutputStream(file), "UTF-8"));
		out.println(expected);
		out.close();
		BufferedReader in = new BufferedReader(new InputStreamReader(
				new FileInputStream(file), "UTF-8"));
		String actual = in.readLine();
		in.close();

		assertEquals(expected, actual);
	}

	public void testPDT287_legacy() throws Exception {
		/*
		 * by default terms shall be canonical, but unquoted, if they are atoms.
		 * This should ensure compatibility with legacy code.
		 */
		PrologSession session = pif.getSession();
		Map<String,Object> map = null;
		try {

			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("{\n", map.get("A"));
		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT291_nil() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.NONE);
		Map<String,Object> m1=session.queryOnce("A=[]");
		assertTrue(m1.get("A") instanceof String);
		session = pif.getSession(PrologInterface.PROCESS_LISTS);
		Map<String,Object> m2=session.queryOnce("A=[]");
		assertTrue(m2.get("A") instanceof List<?>);
		session = pif.getSession(PrologInterface.CTERMS);
		Map<String,Object> m3=session.queryOnce("A=[]");
		assertTrue(m3.get("A") instanceof CNil);
	}
	
	public void testPDT287_0() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.NONE);
		Map<String,Object> map = null;
		try {
			// atoms should be quoted.
			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("'{\\n'", map.get("A"));

			// terms should be canonical.
			map = session.queryOnce("A=('B'+'C')");
			assertEquals("+('B', 'C')", map.get("A"));

			// lists should be ignored
			map = session.queryOnce("A=[1,2]");
			assertEquals("'.'(1, '.'(2, []))", map.get("A"));
		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_1() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.UNQUOTE_ATOMS);
		Map<String,Object> map = null;
		try {
			// atoms should be unquoted.
			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("{\n", map.get("A"));

			// terms should be canonical.
			map = session.queryOnce("A=('B'+'C')");
			assertEquals("+('B', 'C')", map.get("A"));

			// lists should be ignored
			map = session.queryOnce("A=[1,2]");
			assertEquals("'.'(1, '.'(2, []))", map.get("A"));
		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_2() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.PROCESS_LISTS);
		Map<String,Object> map = null;
		try {
			// atoms should be quoted.
			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("'{\\n'", map.get("A"));

			// terms should be canonical.
			map = session.queryOnce("A=('B'+'C')");
			assertEquals("+('B', 'C')", map.get("A"));

			// lists should be processed
			map = session.queryOnce("A=[[1,2],'A']");
			Object o = map.get("A");
			assertTrue(o instanceof List<?>);

			// list elements should be processed recursively
			List<?> l = (List<?>) o;
			assertTrue(l.get(0) instanceof List<?>);
			assertEquals("'A'", l.get(1));

		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_3() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.PROCESS_LISTS
				| PrologInterface.UNQUOTE_ATOMS);
		Map<String,Object> map = null;
		try {
			// atoms should be unquoted.
			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("{\n", map.get("A"));

			// terms should be canonical.
			map = session.queryOnce("A=('B'+'C')");
			assertEquals("+('B', 'C')", map.get("A"));

			// lists should be processed
			map = session.queryOnce("A=[[1,2],'A']");
			Object o = map.get("A");
			assertTrue(o instanceof List<?>);

			// list elements should be processed recursively
			List<?> l = (List<?>) o;
			assertTrue(l.get(0) instanceof List<?>);
			assertEquals("A", l.get(1));

		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_4() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.CTERMS);
		Map<String,Object> map = null;
		try {
			// Everything should be CTerms, no list Processing.
			map = session.queryOnce("A=[[1,2],'A']");
			Object o = map.get("A");
			assertTrue(o instanceof CCompound);

		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_illegal_session() throws Exception {
		// combination of CTERMS and UNQUOTE_ATOMS is illegal.
		try {
			pif.getSession(PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			pif.getSession(PrologInterface.CTERMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			pif.getSession(PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
		
	public void testEscapePDT245() throws Exception {
		PrologSession session = pif.getSession();
		Map<String,Object> map = null;
		try {
			map = session.queryOnce("atom_codes(A,[123,10])");
			assertEquals("{\n", map.get("A"));
		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testException() throws Exception {
		PrologSession session = pif.getSession();
		try {
			session.queryOnce("cluse(a,b)");
			fail("expected exception");
		} catch (Exception e) {
			try {
				session.queryOnce("true");
			} catch (Exception ex) {
				fail("no exception expected");
			}
			session.dispose();
			e.printStackTrace();
		}
	}

	public void test_manySessions() throws Throwable {
		int N = 24;
		PrologSession[] sessions = new PrologSession[N];
		for (int i = 0; i < N; i++) {
			sessions[i] = pif.getSession();
		}
		for (int i = 0; i < N; i++) {
			sessions[i].dispose();
		}
	}
}


