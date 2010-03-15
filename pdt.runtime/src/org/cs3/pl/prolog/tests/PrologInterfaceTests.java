package org.cs3.pl.prolog.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class PrologInterfaceTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.cs3.pl.prolog.tests");
		//$JUnit-BEGIN$
		suite.addTestSuite(XpceTest.class);
		suite.addTestSuite(SocketSessionTest.class);
		suite.addTestSuite(AsyncSocketSessionTest.class);
		//suite.addTestSuite(MysteriousRaceConditionTest.class);
		suite.addTestSuite(SocketSessionThrowTest.class);
		suite.addTestSuite(LifeCycleHookTest.class);
		suite.addTestSuite(LazyStartupRaceTest.class);
		//$JUnit-END$
		return suite;
	}

}
