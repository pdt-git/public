/*
 */
package org.cs3.jlmp.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 */
public class NotAllTests {

    public static Test suite() {
        TestSuite suite = new TestSuite("Test for org.cs3.jlmp.tests");
        //$JUnit-BEGIN$
        suite.addTestSuite(SourceCodeRegeneratorTest.class);
        suite.addTestSuite(BuilderTest.class);
        suite.addTestSuite(PersistenceTest.class);
        suite.addTestSuite(FullQualifiedNameTest.class);
        suite.addTestSuite(NormalizeTest.class);
        suite.addTestSuite(MultiDimensionalArraysTest.class);
        suite.addTestSuite(MultiFragForLoopInitializerTest.class);
        suite.addTestSuite(SuperFieldAccessTest.class);
        suite.addTestSuite(SuperMethodInvocationTest.class);
        suite.addTestSuite(PrimitiveTypeLiteralsTest.class);
        suite.addTestSuite(SelfTest.class);
        suite.addTestSuite(MultiFragForLoopInitializerTest.class);
        suite.addTestSuite(InitializerTest.class);
        suite.addTest(PseudoRoundTripTest.suite());
        //$JUnit-END$
        return suite;
    }
}
