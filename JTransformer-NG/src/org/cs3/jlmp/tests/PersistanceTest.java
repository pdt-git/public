/*
 */
package org.cs3.jlmp.tests;

import java.io.File;
import java.io.FileInputStream;

import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;

/**
 */
public class PersistanceTest extends FactGenerationTest {
    /**
     * @param name
     */
    public PersistanceTest(String name) {
        super(name);
    }
    /* (non-Javadoc)
     * @see org.cs3.jlmp.tests.FactGenerationTest#setUpOnce()
     */
    public void setUpOnce() throws Exception {
        setAutoBuilding(false);
        super.setUpOnce();
    }
    public void testIt() throws Throwable{
        clean();
         PrologInterface pif = getTestJLMPProject().getPrologInterface();
         PrologSession s = pif.getSession();
         File file = File.createTempFile("persistencetest","pl");
         s.queryOnce("assert(toplevelT(la,le,lu,lo))");
         //this caused problems in the past:
         //there was a forgotten deleteSourceFacts in the builder
         build();
         s.queryOnce("writeTreeFacts('"+Util.prologFileName(file)+"')");
         FileInputStream fis = new FileInputStream(file);
         String data = Util.toString(fis);
         fis.close();
         
         String line = "toplevelT(la, le, lu, lo)";
        assertTrue(data.indexOf(line)>-1);
    }
}
