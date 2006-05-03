/*
 */
package org.cs3.jtransformer.tests;

import java.io.File;
import java.util.List;

import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;


/**
 * covers jt-108
 */
public class SuperMethodInvocationTest extends FactGenerationTest {
    public SuperMethodInvocationTest(String name) {
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
            install("jt108");
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testIt() throws CoreException, PrologInterfaceException {
        build();
        PrologSession s =getTestJTransformerProject().getPrologInterface().getSession();
        
        List l = s.queryAll("methodDefT(Bar,T,'bar',_,_,_,_)," +
        		"methodDefT(Baz,X,'baz',_,_,_,_)," +
         		"applyT(Apply,Par,Baz,Select,'bar',Args,Bar)," +
         		"selectT(Select,Apply,Baz,'super',Ident,T)," +
         		"identT(Ident,Select,Baz,'Test',Test)," +
         		"classDefT(Test,_,'Test',_)");
        assertEquals(1,l.size());
    }
}
