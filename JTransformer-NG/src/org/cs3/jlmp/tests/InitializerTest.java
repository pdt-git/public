/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

/**
 * covers JT-103
 */
public class InitializerTest extends FactGenerationTest {
   public InitializerTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        super.setUp();
        //install test workspace
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {            
            
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    /* (non-Javadoc)
     * @see org.cs3.jlmp.tests.SuiteOfTestCases#tearDown()
     */
    protected void tearDown() throws Exception {
        getTestJLMPProject().getPrologInterface().stop();
    }
    public void testPEFs() throws CoreException, IOException {
        //methodDefT(Method,_,'<clinit>',[],null,[],Block),blockT(Block,Method,Method,[NoOp]),nopT(NoOp,Block,Method).
        install("initializer");
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();
        //get  ids
        List l= s.queryAll("methodDefT(Method,_,'<clinit>',[],null,[],Block)," +
        							"blockT(Block,Method,Method,[NoOp])," +
        							"nopT(NoOp,Block,Method)");
        assertEquals(4,l.size());
        Map r = (Map) l.get(0);
        assertNull(s.queryOnce("modifierT("+r.get("Method")+", _)"));
        r = (Map) l.get(1);
        assertNotNull(s.queryOnce("modifierT("+r.get("Method")+", static)"));
        r = (Map) l.get(2);
        assertNull(s.queryOnce("modifierT("+r.get("Method")+", _)"));
        r = (Map) l.get(3);
        assertNotNull(s.queryOnce("modifierT("+r.get("Method")+", static)"));
        
    	uninstall("initializer");
    }
    
   
}
