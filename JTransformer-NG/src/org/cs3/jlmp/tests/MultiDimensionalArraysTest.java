/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

/**
 * covers JT-101
 */
public class MultiDimensionalArraysTest extends FactGenerationTest {
    public MultiDimensionalArraysTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {

        super.setUp();
        //install test workspace
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-roundtrip.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
                "testdata-roundtrip"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {
            /*ld:
             * there were occations where this test would
             * fail when run first and pass when run a secand time.
             */
            if (pif.isUp()) {
                pif.stop();
                pif.getConsultService(JLMP.SRC).clearRecords();
                pif.getConsultService(JLMP.EXT).clearRecords();
            }
            install("test0011");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#tearDown()
     */
    protected void tearDown() throws Exception {
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        pif.stop();
        pif.getConsultService(JLMP.SRC).clearRecords();
        pif.getConsultService(JLMP.EXT).clearRecords();
        super.tearDown();
    }
    public void testIt() throws CoreException {
        build();
        PrologSession s = getTestJLMPProject().getPrologInterface()
                .getSession();

        Map r = s
                .queryOnce("newArrayT(Id,_,_,[DimsId|Tail],Elms,Type),gen_tree(Id,A),gen_tree(DimsId,Dims)");
        assertEquals("3", (String) r.get("Dims"));
        assertEquals(0, ((List) r.get("Tail")).size());
        assertEquals(0, ((List) r.get("Elms")).size());
        assertEquals("type(basic, int, 2)", (String) r.get("Type"));
        assertEquals("new int[3][]", (String) r.get("A"));//This
                                                                       // fails
                                                                       // due to
                                                                       // JT-101
    }

}
