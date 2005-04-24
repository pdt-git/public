package org.cs3.pl.metadata.tests;


import junit.framework.Test;
import junit.framework.TestSuite;

public class MetadataTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Tests for Metadata project");
		//$JUnit-BEGIN$
		suite.addTestSuite(PrologCompilerTest.class);
		suite.addTestSuite(StringLineBreakInfoProviderTest.class);
		//$JUnit-END$
		return suite;
	}

}
