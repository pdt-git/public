package org.cs3.pl.parser;

import junit.framework.Test;
import junit.framework.TestSuite;

public class ParserTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.cs3.pl.parser");
		//$JUnit-BEGIN$
		suite.addTestSuite(SyntaxTest.class);
		suite.addTestSuite(StringLineBreakInfoProviderTest.class);
		suite.addTest(ElementDataTest.suite());
		//$JUnit-END$
		return suite;
	}

}
