/*
 */
package org.cs3.pl.prolog.internal;

import java.io.File;
import java.io.FileReader;
import java.io.PrintStream;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.DefaultResourceFileLocator;
import org.cs3.pl.prolog.ConsultService;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;

/**
 */
public class ConsultServiceTest extends TestCase {

    private String prefix;
    private ConsultService rcs;
    private PrologInterface pif;

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        Debug.setDebugLevel(Debug.LEVEL_DEBUG);
        final int port = 5624;
        super.setUp();

        File prefix = new File(System.getProperty("java.io.tmpdir"));
        
        Debug.info("prefix: " + prefix.getCanonicalPath());
        PrologInterfaceFactory factory = PrologInterfaceFactory.newInstance();
        factory.setResourceLocator(new DefaultResourceFileLocator(prefix));
        pif = factory.create();
        pif.start();
        rcs = pif.getConsultService("");
    }

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {  
        super.tearDown();
        pif.stop();
    }

   
    
    public void testConsult() throws Throwable {
        String text = ":-dynamic wahr/1.\n" + ":-assert(wahr(wahrheit)).\n"
                + "auch_wahr(A) :-\n" + "\twahr(A).";
        PrintStream s = rcs.getOutputStream("/fnord/fnum");
        assertTrue(s != null);
        s.print(text);
        s.close();

        //see if the fact has been asserted
//        PrologSession session = pif.getSession();
//        Hashtable r = session.query("wahr(wahrheit)");
//        assertTrue(r != null);
//        r = session.query("auch_wahr(wahrheit)");
//        assertTrue(r != null);
//        r = session.query("consulted_symbol(A)");
//        Debug.debug("consulted_symbol(A)");
//        Debug.debug(Util.prettyPrint(r));
        assertTrue(rcs.isConsulted("/fnord/fnum"));

        // Debug.debug()
        //see if the file has been created

        String filename = System.getProperty("java.io.tmpdir") + File.separator
                + "fnord" + File.separator + "fnum";
        File file = new File(filename);
        assertTrue("Does not exist: "+filename,file.exists());
        assertTrue("Can not read: "+filename,file.canRead());

        //check the contents of that file
        FileReader reader = new FileReader(file);
        StringBuffer sb = new StringBuffer();
        int read = reader.read();
        while (read != -1) {
            sb.append((char) read);
            read = reader.read();
        }
        reader.close();

        assertEquals(text, sb.toString());
//        session.dispose();

    }

    public void testUnconsult() throws Throwable {
       testConsult();
       testConsult();
        rcs.unconsult("/fnord/fnum");
        //should be "No"

        assertFalse(rcs.isConsulted("/fnord/fnum"));
        String filename = System.getProperty("java.io.tmpdir") + File.separator
                + "fnord" + File.separator + "fnum";
        File file = new File(filename);
        assertFalse(file.exists());

//        PrologSession session = pif.getSession();
//        Hashtable r = session.query("wahr(wahrheit)");
//        assertTrue(r == null);
//        PrologException e=null;
//        try{
//            r = session.query("auch_wahr(wahrheit)");
//            fail();
//        }
//        catch(PrologException se){
//            e=se;
//            Debug.report(e);
//        }
//        catch(Throwable t){
//            Debug.report(t);
//            fail( );
//        }
//        assertTrue(e != null);
        testConsult();
    }

    public void testReconsult() throws Throwable {
        for (int i = 0; i < 1; i++) {
            testConsult();
            String text = ":-dynamic wahr/1.\n"
                    + ":-assert(wahr(nichtfalsch)).\n" + "auch_wahr(X) :-\n"
                    + "\twahr(_A),atom_concat(nicht,X,_A).";

            PrintStream s = rcs.getOutputStream("/fnord/fnum");
            assertTrue(s != null);
            s.print(text);
            s.close();
            assertTrue(rcs.isConsulted("/fnord/fnum"));
            String filename = System.getProperty("java.io.tmpdir")
                    + File.separator + "fnord" + File.separator + "fnum";

//            PrologSession session = pif.getSession();
//            Hashtable r = session.query("wahr(wahrheit)");
//            assertTrue(r == null);
//            r = session.query("wahr(nichtfalsch)");
//            assertTrue(r != null);
//
//            r = session.query("auch_wahr(wahrheit)");
//            assertTrue(r == null);
//            r = session.query("auch_wahr(falsch)");
//            assertTrue(r != null);

            File file = new File(filename);
            assertTrue(file.exists());

            //check the contents of that file
            FileReader reader = new FileReader(file);
            StringBuffer sb = new StringBuffer();
            int read = reader.read();
            while (read != -1) {
                sb.append((char) read);
                read = reader.read();
            }
            reader.close();

            assertEquals(text, sb.toString());
//            session.dispose();
        }
    }
}
