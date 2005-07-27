package org.cs3.pl.prolog.internal;

import junit.framework.Test;
import junit.framework.TestSuite;

public class NotAllPrologTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.cs3.pl.prolog.internal");
		//$JUnit-BEGIN$
		suite.addTestSuite(ConsultServiceTest.class);
		suite.addTestSuite(PrologSessionTest.class);
		suite.addTestSuite(PrologSessionThrowTest.class);
		//$JUnit-END$
		return suite;
	}

}
