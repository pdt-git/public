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
public class SuperFieldAccessTest extends FactGenerationTest {
   public SuperFieldAccessTest(String name) {
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
    protected void _tearDown() throws Exception {
        getTestJLMPProject().getPrologInterface().stop();
    }
    public void testIt() throws CoreException, IOException {
        install("jt103");
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();
        //get  ids
        Map r = s.queryOnce("fieldDefT(Id,Class,_,'ja',_)");
        String jaFieldId = (String) r.get("Id");
        String pumpelClassId=(String)r.get("Class");
        assertNotNull(jaFieldId);
        assertNotNull(pumpelClassId);
        
        r = s.queryOnce("fieldDefT(Id,Class,_,'nein',_)");
        String neinFieldId = (String) r.get("Id");
        String umpfClassId=(String)r.get("Class");
        assertNotNull(neinFieldId);
        assertNotNull(umpfClassId);
        
        List l = s.queryAll("getFieldT(Id,_,Enc,Exp,Nme,Fld)," +
        		"methodDefT(Enc,_,'foo',_,_,_,_)," +
        		"identT(Exp,_,_,'this',Sym)");
        assertEquals(1,l.size());
         r = (Map) l.get(0);
         assertEquals("ja",(String)r.get("Nme"));
         assertEquals(jaFieldId,(String)r.get("Fld"));
         assertEquals(pumpelClassId,(String)r.get("Sym"));
         
          l = s.queryAll("getFieldT(Id,_,Enc,'null',Nme,Fld)," +
         		"methodDefT(Enc,_,'bar',_,_,_,_)");
         assertEquals(1,l.size());
          r = (Map) l.get(0);
          assertEquals("ja",(String)r.get("Nme"));          
          assertEquals(jaFieldId,(String)r.get("Fld"));
          
          
          l = s.queryAll("getFieldT(Id,_,Enc,'null',Nme,Fld)," +
   		"methodDefT(Enc,_,'baz',_,_,_,_)");
          assertEquals(1,l.size());
          r = (Map) l.get(0);
          assertEquals("nein",(String)r.get("Nme"));
          assertEquals(neinFieldId,(String)r.get("Fld"));
          
          //the following should fail due to jt-103
           l = s.queryAll("getFieldT(Id,_,Enc,Exp,Nme,Fld)," +
          		"methodDefT(Enc,_,'bang',_,_,_,_)," +
          		"identT(Exp,_,_,'super',Sym)");
            assertEquals(1,l.size());
            r = (Map) l.get(0);
            assertEquals("nein",(String)r.get("Nme"));
            assertEquals(neinFieldId,(String)r.get("Fld"));
            assertEquals(umpfClassId,(String)r.get("Sym"));
            uninstall("jt103");
    }
    
    /**
     * i think jt108 could also affect super field access expressions.
     * This should cover it. 
     * @throws CoreException
     * @throws IOException
     */
    public void testQualifiedSuperFieldAccess() throws CoreException, IOException {
        install("jt108b");
        build();
        PrologSession s =getTestJLMPProject().getPrologInterface().getSession();
        
        List l = s.queryAll("fieldDefT(Bar,T,_,'bar',_)," +
        		"methodDefT(Baz,X,'baz',_,_,_,_)," +
         		"getFieldT(Apply,Par,Baz,Select,'bar',Bar)," +
         		"selectT(Select,Apply,Baz,'super',Ident,T)," +
         		"identT(Ident,Select,Baz,'Test',Test)," +
         		"classDefT(Test,_,'Test',_)");
        assertEquals(1,l.size());
        uninstall("jt108b");
    }
}
