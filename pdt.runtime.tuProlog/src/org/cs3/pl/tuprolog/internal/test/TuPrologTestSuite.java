package org.cs3.pl.tuprolog.internal.test;

import junit.framework.Test;
import junit.framework.TestSuite;

public class TuPrologTestSuite {

	public static Test suite() {
		TestSuite suite = new TestSuite(
				"Test for org.cs3.pl.tuprolog.internal.test");
		//$JUnit-BEGIN$
		suite.addTestSuite(TuPrologTest.class);
		suite.addTestSuite(SocketSessionTest.class);

		suite.addTestSuite(LazyStartupRaceTest.class);
		suite.addTestSuite(TuPrologSessionTest.class);

		
		suite.addTestSuite(SWICompatibilityLibraryTest.class);
		suite.addTestSuite(SyncTest.class);
		suite.addTestSuite(ObserverLibraryTest.class);
		
		//$JUnit-END$
		return suite;
	}

}
