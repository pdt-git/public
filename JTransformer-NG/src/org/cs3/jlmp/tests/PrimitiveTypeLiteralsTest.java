 /*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.util.Map;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;

/**
 * covers JT-34
 */
public class PrimitiveTypeLiteralsTest extends FactGenerationTest {
   public PrimitiveTypeLiteralsTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        super.setUp();
        //install test workspace
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-roundtrip.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator("testdata-roundtrip"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {            
            install("test0038");//actualy we only need java.lang.Class and friends.
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testIt() throws CoreException {
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();
        
        Map r = s.queryOnce("open_printf_to_memory," +
        		"classDefT(CID,_,'Class',_)," +
        		"gen_literal(type(class, CID, 0)," +
        		"type(basic,int,0))," +
        		"close_printf_to_memory(SRC)");
        
        assertEquals("int.class",(String)r.get("SRC"));
        
         r = s.queryOnce("open_printf_to_memory," +
        		"classDefT(CID,_,'Class',_)," +
        		"gen_literal(type(class, CID, 0)," +
        		"type(basic,int,1))," +
        		"close_printf_to_memory(SRC)");
        
        assertEquals("int[].class",(String)r.get("SRC"));
        
        r = s.queryOnce("open_printf_to_memory," +
        		"classDefT(CID,_,'Class',_)," +
        		"gen_literal(type(class, CID, 0)," +
        		"type(class,CID,1))," +
        		"close_printf_to_memory(SRC)");
        
        assertEquals("java.lang.Class[].class",(String)r.get("SRC"));
        r = s.queryOnce("open_printf_to_memory," +
        		"classDefT(CID,_,'Class',_)," +
        		"gen_literal(type(class, CID, 0)," +
        		"type(basic,int,3))," +
        		"close_printf_to_memory(SRC)");
        
        assertEquals("int[][][].class",(String)r.get("SRC"));
        
        r = s.queryOnce("open_printf_to_memory," +
        		"classDefT(CID,_,'Class',_)," +
        		"gen_literal(type(class, CID, 0)," +
        		"type(class,CID,3))," +
        		"close_printf_to_memory(SRC)");
        
        assertEquals("java.lang.Class[][][].class",(String)r.get("SRC"));
    }
    
}
