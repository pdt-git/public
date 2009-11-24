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

import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CNil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

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
	protected void setUp() throws Exception {
		Debug.setDebugLevel("DEBUG");

//		pif = PrologInterfaceFactory.newInstance().create();
		pif = AbstractPrologInterface.newInstance();

		pif.start();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see junit.framework.TestCase#tearDown()
	 */
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
		Map map = s.queryOnce("A=[1,2]");
		assertNotNull(map);
		Object a = map.get("A");
		assertTrue(a instanceof CCompound);

	}

	public void testListProcessingDisabled() throws PrologException,
			PrologInterfaceException {
		PrologSession s =  pif.getSession(PrologInterface.NONE);
		
		Map map = s.queryOnce("A=[1,2]");
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
		Map r = server.queryOnce("assert(wahr(wahrheit))");
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
			// s.queryOnce("guitracer");
			// s.queryOnce("spy(handle_command)");
			// s.queryAll("test ( bla )");
		} catch (PrologException e) {
			;
		}
	}

	public void testIOProblem() throws Throwable {
		final PrologSession s = pif.getSession();
		final Throwable[] t = new Throwable[1];
		Thread thread = new Thread() {

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

		// ((PIFComPrologInterface)pif).process.destroy();
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
		Map map = s.queryOnce("A=[1,2,3,[[a,b,['{}']]],[b,c]]");

		Object A = map.get("A");
		assertEquals("[1, 2, 3, [[a, b, [{}]]], [b, c]]", A.toString());
		assertTrue(A instanceof List);
		List l = (List) A;
		assertEquals("1", (String) l.get(0));
		assertEquals("2", (String) l.get(1));
		assertEquals("3", (String) l.get(2));
		assertEquals(5, l.size());
		A = l.get(3);
		assertTrue(A instanceof List);
		List m = (List) A;
		assertEquals(1, m.size());
		assertTrue(m.get(0) instanceof List);
		m = (List) m.get(0);
		assertEquals(3, m.size());
		assertEquals("a", (String) m.get(0));
		assertEquals("b", (String) m.get(1));
		assertTrue(m.get(2) instanceof List);
		m = (List) m.get(2);
		assertEquals(1, m.size());
		assertEquals("{}", (String) m.get(0));
		assertTrue(l.get(4) instanceof List);
		m = (List) l.get(4);
		assertEquals(2, m.size());
		assertEquals("b", (String) m.get(0));
		assertEquals("c", (String) m.get(1));

	}

	public void testQueryAll() throws Throwable {
		PrologSession s = pif.getSession();

		List l = s.queryAll("member(A,[ich,du,muellers_kuh])");
		assertEquals(3, l.size());
		Vector v = new Vector();
		for (Iterator it = l.iterator(); it.hasNext();) {
			Map m = (Map) it.next();
			v.add(m.get("A"));
		}
		assertEquals("ich", v.get(0));
		assertEquals("du", v.get(1));
		assertEquals("muellers_kuh", v.get(2));

		l = s.queryAll("member(ich,[ich,ich,muellers_kuh])");
		assertEquals(2, l.size());
		for (Iterator it = l.iterator(); it.hasNext();) {
			Map m = (Map) it.next();
			assertEquals(0, m.size());
		}
		l = s.queryAll("member(und,[ich,du,muellers_kuh])");
		assertEquals(0, l.size());

	}

	public void _test_longAtomLength() throws Throwable {
		// session.queryOnce("0", "guitracer");
		// session.queryOnce("0", "trace");
		// session.join();
		// rec.clear();
		StringBuffer sb = new StringBuffer();

		int len = 600000;
		for (int i = 0; i < len; i++) {
			sb.append('a');
		}

		String atom = sb.toString();
		PrologSession s = pif.getSession();
		Map map = s.queryOnce("atom_length(" + atom + ",1,3,_,Sub)");
		assertEquals("aaa", map.get("Sub"));
	}

	public void _test_longAtom_result() throws Throwable {
		// session.queryOnce("0", "guitracer");
		// session.queryOnce("0", "trace");
		// session.join();
		// rec.clear();
		StringBuffer sb = new StringBuffer();

		for (int i = 0; i < 600000; i++) {
			sb.append('a');
		}

		String atom = sb.toString();
		PrologSession s = pif.getSession();
		Map map = s.queryOnce("A=" + atom);
		assertEquals(atom, map.get("A"));
	}

	public void test_longAtom() throws Throwable {
		// session.queryOnce("0", "guitracer");
		// session.queryOnce("0", "trace");
		// session.join();
		// rec.clear();
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
		// session.queryOnce("guitracer");
		// session.queryOnce("trace");
		String query = "A=kad\u0131n(nurhan)";
		Map map = session.queryOnce(query);
		String actual = (String) map.get("A");
		assertEquals("kad\u0131n(nurhan)", actual);
	}

	public void testTurkishX() throws Exception {

		// session.queryOnce("guitracer");
		// session.queryOnce("trace");
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
		Map map = null;
		try {
			// map = session.queryOnce("A = 'package test0001;\n\nimport
			// java.util.*;\n\npublic class Test {\n'");
			// session.queryOnce("guitracer");
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
		Map m1=session.queryOnce("A=[]",PrologInterface.NONE);
		Map m2=session.queryOnce("A=[]",PrologInterface.PROCESS_LISTS);
		Map m3=session.queryOnce("A=[]",PrologInterface.CTERMS);
		
		assertTrue(m1.get("A") instanceof String);
		assertTrue(m2.get("A") instanceof List);
		assertTrue(m3.get("A") instanceof CNil);
	}
	
	public void testPDT287_0() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.NONE);
		Map map = null;
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
		Map map = null;
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
		Map map = null;
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
			assertTrue(o instanceof List);

			// list elements should be processed recursively
			List l = (List) o;
			assertTrue(l.get(0) instanceof List);
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
		Map map = null;
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
			assertTrue(o instanceof List);

			// list elements should be processed recursively
			List l = (List) o;
			assertTrue(l.get(0) instanceof List);
			assertEquals("A", l.get(1));

		} catch (Exception e) {
			session.dispose();
			fail();
			e.printStackTrace();
		}
	}

	public void testPDT287_4() throws Exception {
		PrologSession session = pif.getSession(PrologInterface.CTERMS);
		Map map = null;
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
			PrologSession session = pif.getSession(PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			PrologSession session = pif.getSession(PrologInterface.CTERMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			PrologSession session = pif.getSession(PrologInterface.CTERMS
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
			
			pif.getSession(PrologInterface.NONE).queryOnce("syntax error",PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		// combination of CTERMS and PROCESS_LIST is illegal (for now).
		try {
			pif.getSession(PrologInterface.NONE).queryOnce("syntax error",PrologInterface.CTERMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
		
		// naturally, combination of all three is illegal
		try {
			pif.getSession(PrologInterface.NONE).queryOnce("syntax error",PrologInterface.CTERMS
					| PrologInterface.UNQUOTE_ATOMS
					| PrologInterface.PROCESS_LISTS);
			fail();
		} catch (IllegalArgumentException e) {
			;
		}
	}
	
	
	public void testEscapePDT245() throws Exception {
		PrologSession session = pif.getSession();
		Map map = null;
		try {
			// map = session.queryOnce("A = 'package test0001;\n\nimport
			// java.util.*;\n\npublic class Test {\n'");
			// session.queryOnce("guitracer");
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
		Map map = null;
		try {
			map = session.queryOnce("cluse(a,b)");
			fail("expected exception");
		} catch (Exception e) {
			try {
				map = session.queryOnce("true"); // <--- blocks the system
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
