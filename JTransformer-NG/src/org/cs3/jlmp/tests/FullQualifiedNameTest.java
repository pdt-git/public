/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;


/**
 * covers jt-107
 */
public class FullQualifiedNameTest extends FactGenerationTest {
    public FullQualifiedNameTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        super.setUp();
    
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {            
            install("jt107");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testIt() throws CoreException {
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();
        
        List l = s.queryAll("classDefT(Cid,_,'Test',_),fullQualifiedName(Cid,Fqn)");
        assertEquals(1,l.size());
        Map r = (Map) l.get(0);
        assertEquals("jt107.Test",(String)r.get("Fqn"));
        
        l = s.queryAll("classDefT(Cid,_,'Object',_),fullQualifiedName(Cid,Fqn)");
        assertEquals(1,l.size());
         r = (Map) l.get(0);
        assertEquals("java.lang.Object",(String)r.get("Fqn"));
    }
}
