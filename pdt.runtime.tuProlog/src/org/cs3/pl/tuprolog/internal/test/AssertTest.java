package org.cs3.pl.tuprolog.internal.test;

import junit.framework.TestCase;
import alice.tuprolog.Prolog;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Theory;

public class AssertTest extends TestCase {

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	public void testOps() throws Exception {
        Prolog engine = new Prolog();
        String theory =
                "ops(s).\n" +
                "ops(y).\n" +
                "ops(z).";
        engine.addTheory(new Theory(theory));
        SolveInfo res = engine.solve("(ops(A), assert(ops_result(A))).");
        System.out.println("res = " + res);
        while (engine.hasOpenAlternatives()) {
            res= engine.solveNext();
            System.out.println("res = " + res);
        }
        
        SolveInfo assert_res = engine.solve("ops_result(X).");
        System.out.println("assert_res = " + assert_res);
        while (engine.hasOpenAlternatives()) {
        	assert_res= engine.solveNext();
            System.out.println("assert_res = " + assert_res);
        }

	}
}
