/*
 */
package org.cs3.jtransformer.tests;

import java.io.File;
import java.util.Iterator;
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
 * covers jt-107
 */
public class FullQualifiedNameTest extends FactGenerationTest {
    public FullQualifiedNameTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        super.setUp();
    
        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator("testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        try {            
            install("jt107");
            pif.start();
            build(IncrementalProjectBuilder.CLEAN_BUILD);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testIt() throws CoreException, PrologInterfaceException {
        build();
        PrologSession s =getTestJTransformerProject().getPrologInterface().getSession();
        List l = s.queryAll("classDefT(Cid,_,'Test',_)");
        for (Iterator iter = l.iterator(); iter.hasNext();) {
            Map m = (Map) iter.next();
            System.out.println(m.get("Cid"));
        }
        //assertEquals(1,l.size());
         l = s.queryAll("classDefT(Cid,_,'Test',_),fullQualifiedName(Cid,Fqn)");
         for (Iterator iter = l.iterator(); iter.hasNext();) {
             Map m = (Map) iter.next();
             System.out.println(m.get("Cid")+"->"+m.get("Fqn"));
         }
         assertEquals(1,l.size());
        Map r = (Map) l.get(0);
        assertEquals("jt107.Test",(String)r.get("Fqn"));
        
        l = s.queryAll("classDefT(Cid,_,'Object',_),fullQualifiedName(Cid,Fqn)");
        assertEquals(1,l.size());
         r = (Map) l.get(0);
        assertEquals("java.lang.Object",(String)r.get("Fqn"));
    }
}
