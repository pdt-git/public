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
public class TypeParameterTest extends FactGenerationTest {
    public TypeParameterTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {
        
        setAutoBuilding(false);
        super.setUp();
//        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
//        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator("testdata-facts"));
//        Util.unzip(r);
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        try {            
            install("jt5_101");
            pif.start();
            build(IncrementalProjectBuilder.CLEAN_BUILD);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }       
    }
    
    public void testIt() throws CoreException, PrologInterfaceException {
        build();
        PrologSession s =getTestJTransformerProject().getPrologInterface().getSession();
        //List l = s.queryAll("gen_toplevels");
        List l = s.queryAll("globalIds(A,B)");
        System.out.println(l);
        System.out.println(" -------------- fields ---------------------");
        l = s.queryAll("globalIds(A,B,C)");
        System.out.println(" -------------- methods ---------------------");
        l = s.queryAll("globalIds(A,B,C,D)");
        
        s.dispose();
    }
}
