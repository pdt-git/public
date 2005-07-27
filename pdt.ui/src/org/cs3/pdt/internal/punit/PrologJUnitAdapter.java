/*
 * Created on 17.03.2004
 *
 * To change the template for this generated file go to
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package org.cs3.pdt.internal.punit;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import junit.framework.TestSuite;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;



/**
 * @author speicher
 * 
 * A simple Testcase which enumerates all <code>test/1</code> clauses and
 * creates a suite of Testcases. One for each clause. To add your test <code>mytest</code>
 * simply write a predicate <code>test('mytest')</code> which succeeds if the
 * test succeeds.
 */
public class PrologJUnitAdapter extends TestPredicateTest {
	
//	/**
//	 * Retrieve a free server port >= port.
//	 * @param port
//	 * @return
//	 */
//	
//	static private int getFreeServerSocket(int port) {
//		while (true)
//			try {
//				ServerSocket sock = new ServerSocket(port);
//				sock.close();
//				return port;
//			} catch (IOException e) {
//				port++;
//			}
//	}

	/**
	 * Generates a Testsuite containing one TestCase for each
	 * test('&lt;testname&gt;') clause in the engine.  
	 * 
	 * @return the generated TestSuite
	 * @throws IOException
	 */
	static PrologInterface pif; 
	
	public static TestSuite suite() throws IOException {
		
		PrologSession session = null;
		TestSuite suite = null;
		try {
			PDTPlugin r = PDTPlugin.getDefault();
			pif = PrologRuntimePlugin.getDefault().getPrologInterface();
			session = pif.getSession();
			// result = manager.query("clause(test(Testname), _)");
			suite = new TestSuite();
			IPrologInterfaceAdapter adapter = new IPrologInterfaceAdapter() {
				public PrologInterface getPrologInterface() {
					return pif;
				}
			};
			List tests = session.queryAll("test_suite(Testname)");
			for (Iterator iter = tests.iterator(); iter.hasNext();) {
				Map test = (Map) iter.next();
				String testname = (String) test.get("Testname");
				suite.addTest(new TestPredicateTest(testname,adapter));
			}
		} finally {
			if (session != null)
				session.dispose();
		}
		
		return suite;
	}

}
