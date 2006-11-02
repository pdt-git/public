package org.cs3.pl.tuprolog.internal.test;

import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;
import junit.framework.TestCase;


public class TestAssert extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	public void testAssert() throws Exception{
		Theory th = new Theory(" addss(X):- nonvar(X), assert(X).");
		Prolog engine = new Prolog();
		engine.addTheory(th);
		
		SolveInfo info = engine.solve("addss(ab).");
		
		info = engine.solve("ab.");
		assertTrue(info.isSuccess() );
			
		
	}
	protected void tearDown() throws Exception {
		super.tearDown();
	}

}
