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
 * covers JT-34
 */
public class TypeTestTest extends FactGenerationTest {
    public TypeTestTest(String name) {
        super(name);

    }

    public void setUp() throws Exception {

        super.setUp();
        // install test workspace
        ResourceFileLocator l = JTransformerPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JTransformerPlugin.getDefault().getResourceLocator(
                "testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJTransformerProject().getPrologInterface();
        try {
            install("jt136");// actualy we only need java.lang.Class and
                                // friends.
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void testIt() throws CoreException, PrologInterfaceException {
        clean();
        build();
        PrologSession s = getTestJTransformerProject().getPrologInterface()
                .getSession();

        List l = s.queryAll("toplevelT(A,B,C,D)");
        assertEquals(1, l.size());
        l = s.queryAll("typeTestT(_,_,_,type(class,C,0),_),"
                + "classDefT(C,_,'String',_)");
        s.dispose();
        assertEquals(1, l.size());
    }

}
