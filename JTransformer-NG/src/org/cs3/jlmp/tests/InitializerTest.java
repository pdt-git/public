/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

/**
 * covers JT-103
 */
public class InitializerTest extends FactGenerationTest {
    private boolean passed;

    public InitializerTest(String name) {
        super(name);

    }

    public void setUpOnce() throws Exception {
        setAutoBuilding(false);
        super.setUpOnce();
        // install test workspace
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
                "testdata-facts"));
        Util.unzip(r);
        
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {

            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#setUp()
     */
    protected void setUp() throws Exception {
        Debug.debug("setting up testPEFS");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
                "testdata-facts"));
        super.setUp();
        passed = false;
    }

//    public static Test suite() {
//        TestSuite s = new TestSuite();
//        s.addTest(new InitializerTest("testPEFs"));
//        s.addTest(new InitializerTest("testPEFs"));
//        s.addTest(new InitializerTest("testPEFs"));
//        
//
//        return s;
//    }

    /*
     * (non-Javadoc)
     * 
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#tearDown()
     */
    public void tearDownOnce() {
        super.tearDownOnce();
        try {
            getTestJLMPProject().getPrologInterface().stop();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public void tearDown() {
        Debug.debug("tearing down testPEFS: " + (passed ? "passed" : "failed"));
    }

    public void testPEFs() throws CoreException, IOException {

        // methodDefT(Method,_,'<clinit>',[],null,[],Block),blockT(Block,Method,Method,[NoOp]),nopT(NoOp,Block,Method).
        install("initializer");
        clean();
        build();
        PrologSession s = getTestJLMPProject().getPrologInterface()
                .getSession();
        // get ids
        List l = s.queryAll("methodDefT(Method,_,'<clinit>',[],null,[],Block),"
                + "blockT(Block,Method,Method,[NoOp]),"
                + "nopT(NoOp,Block,Method)");
        assertEquals(4, l.size());
        Map r = (Map) l.get(0);
        assertNull(s.queryOnce("modifierT(" + r.get("Method") + ", _)"));
        r = (Map) l.get(1);
        assertNotNull(s.queryOnce("modifierT(" + r.get("Method") + ", static)"));
        r = (Map) l.get(2);
        assertNull(s.queryOnce("modifierT(" + r.get("Method") + ", _)"));
        r = (Map) l.get(3);
        // Map rr= s.queryOnce("modifierT("+r.get("Method")+", static)");
        
        assertNotNull(s.queryOnce("modifierT(" + r.get("Method") + ", static)"));

        uninstall("initializer");
        passed = true;
    }

}
