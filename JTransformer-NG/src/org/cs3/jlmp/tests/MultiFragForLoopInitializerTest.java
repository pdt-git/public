 /*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.util.Map;

import junit.framework.Test;
import junit.framework.TestSuite;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.ICompilationUnit;

/**
 * covers JT-118
 */
public class MultiFragForLoopInitializerTest extends FactGenerationTest {
   public MultiFragForLoopInitializerTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        super.setUp();
        Debug.setDebugLevel("DEBUG");
        //install test workspace
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-roundtrip.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata-roundtrip"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {            
            install("test0360");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    public static Test suite(){
        TestSuite s = new TestSuite();
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        s.addTest(new  MultiFragForLoopInitializerTest("testIt"));
        return s;
    }
    public void testIt() throws CoreException, InterruptedException {
        ICompilationUnit[] cus = getCompilationUnitsInFolder("test0360");
        assertFalse(cus.length==0);
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();        
        try {
            Map r = s.queryOnce("toplevelT(A,B,C,D)");
            assertNotNull(r);
            r = s.queryOnce("open_printf_to_memory,"
                    + "forLoopT(_,_,_,Inits,_,_,_),"
                    + "gen_komma_list_inits(Inits),"
                    + "close_printf_to_memory(SRC)");
            assertNotNull(r);
            assertEquals("int i = 0, j = 0, k = 0", (String) r.get("SRC"));
        }finally{
            s.dispose();
        }
    }
    
}
