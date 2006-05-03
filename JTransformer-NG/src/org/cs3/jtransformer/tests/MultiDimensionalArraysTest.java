/*
 */
package org.cs3.jtransformer.tests;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IncrementalProjectBuilder;
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
        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-roundtrip.zip");
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator(
                "testdata-roundtrip"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        try {
            /*ld:
             * there were occations where this test would
             * fail when run first and pass when run a secand time.
             */
            if (pif.isUp()) {
                getTestProject().build(IncrementalProjectBuilder.CLEAN_BUILD,null);
                pif.stop();
               
            }
            install("test0011");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    /* (non-Javadoc)
     * @see org.cs3.jtransformer.tests.SuiteOfTestCases#tearDown()
     */
    protected void tearDown() throws Exception {
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        getTestProject().build(IncrementalProjectBuilder.CLEAN_BUILD,null);
        pif.stop();
        super.tearDown();
    }
    public void testIt() throws CoreException, PrologInterfaceException {
        build();
        PrologSession s = getTestJTransformerProject().getPrologInterface()
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
