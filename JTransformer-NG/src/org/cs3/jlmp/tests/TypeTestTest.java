/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.util.List;

import org.cs3.jlmp.JLMPPlugin;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
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
        ResourceFileLocator l = JLMPPlugin.getDefault().getResourceLocator("");
        File r = l.resolve("testdata-facts.zip");
        setTestDataLocator(JLMPPlugin.getDefault().getResourceLocator(
                "testdata-facts"));
        Util.unzip(r);
        setAutoBuilding(false);
        PrologInterface pif = getTestJLMPProject().getPrologInterface();
        try {
            install("jt136");// actualy we only need java.lang.Class and
                                // friends.
            pif.start();
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    public void testIt() throws CoreException {
        clean();
        build();
        PrologSession s = getTestJLMPProject().getPrologInterface()
                .getSession();

        List l = s.queryAll("toplevelT(A,B,C,D)");
        assertEquals(1, l.size());
        l = s.queryAll("typeTestT(_,_,_,type(class,C,0),_),"
                + "classDefT(C,_,'String',_)");
        s.dispose();
        assertEquals(1, l.size());
    }

}
