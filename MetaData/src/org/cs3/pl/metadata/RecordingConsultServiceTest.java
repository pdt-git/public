/*
 */
package org.cs3.pl.metadata;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Hashtable;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.SessionException;

/**
 */
public class RecordingConsultServiceTest extends TestCase {
    PrologInterface pif;

    RecordingConsultService rcs;

    private String prefix;

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
        Debug.setDebugLevel(Debug.LEVEL_DEBUG);
        final int port = 5624;
        super.setUp();

        final String engine = System.getProperty("engine.dir");
        Debug.info("engine: " + engine);
        //find tempdir and (hopefully :-) )convert to POSIX path
        File prefix = new File(System.getProperty("java.io.tmpdir"));
        
        Debug.info("prefix: " + prefix.getCanonicalPath());

        /*-Ddebug.level=debug 
         * -Dpdt.swipl.dir="/usr/local/lib/swi-prolog"
         *  -Dpdt.use.session.pooling="true"*/
        pif = new PrologInterface();
        //pif.setStartStrategy(new InProcessStartStrategy());
        pif.setPort(4711);
        pif.setStandAloneServer(false);
        pif.setUseSessionPooling(true);
        pif.addLifeCycleHook(new LifeCycleHook() {
            public void onInit(PrologSession s) {
                s.consult(engine + "/main.pl");
                if (Util.probePort(port)) {
                    Debug
                            .info("Consult server thread seems to be running, so i will not start a new one.");
                } else {
                    String queryString = "consult_server(" + port + ")";
                    Debug.info("starting consult server using: " + queryString);
                    try {
                        if (s.query(queryString) == null) {
                            Debug.error("could not start!");
                        }

                    } catch (SessionException e) {
                        Debug.report(e);
                    }
                    while (!Util.probePort(port)) {
                        try {
                            Thread.sleep(50);
                        } catch (InterruptedException e1) {
                            Debug.report(e1);
                        }
                    }
                    Debug.debug("Server thread created");
                    try {
                        rcs.connect();
                    } catch (IOException e1) {
                        Debug.report(e1);
                    }
                }

            }

            public void afterInit() {

            }

            public void beforeShutdown(PrologSession session) {
                rcs.disconnect();
            }
        });
        rcs = new RecordingConsultService();
        rcs.setPort(port);
        rcs.setPrefix(prefix);
        rcs.setPrologInterface(pif);

        pif.start();
    }

    /*
     * (non-Javadoc)
     * 
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        pif.stop();
        super.tearDown();
    }

    public void testConsult() throws Throwable {
        String text = ":-dynamic wahr/1.\n" + ":-assert(wahr(wahrheit)).\n"
                + "auch_wahr(A) :-\n" + "\twahr(A).";
        PrintStream s = rcs.getOutputStream("/fnord/fnum");
        assertTrue(s != null);
        s.print(text);
        s.close();

        //see if the fact has been asserted
        PrologSession session = pif.getSession();
        Hashtable r = session.query("wahr(wahrheit)");
        assertTrue(r != null);
        r = session.query("auch_wahr(wahrheit)");
        assertTrue(r != null);
        r = session.query("consulted_symbol(A)");
        Debug.debug("consulted_symbol(A)");
        Debug.debug(Util.prettyPrint(r));
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
        session.dispose();

    }

    public void _testUnconsult() throws Throwable {
       testConsult();
        rcs.unconsult("/fnord/fnum");
        //should be "No"

        assertFalse(rcs.isConsulted("/fnord/fnum"));
        String filename = System.getProperty("java.io.tmpdir") + File.separator
                + "fnord" + File.separator + "fnum";
        File file = new File(filename);
        assertFalse(file.exists());

        PrologSession session = pif.getSession();
        Hashtable r = session.query("wahr(wahrheit)");
        assertTrue(r == null);
        try{
            r = session.query("auch_wahr(wahrheit)");
            fail();
        }
        catch(SessionException se){
            ;
        }
        //assertTrue(r == null);
    }

    public void _testReconsult() throws Throwable {
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

            PrologSession session = pif.getSession();
            Hashtable r = session.query("wahr(wahrheit)");
            assertTrue(r == null);
            r = session.query("wahr(nichtfalsch)");
            assertTrue(r != null);

            r = session.query("auch_wahr(wahrheit)");
            assertTrue(r == null);
            r = session.query("auch_wahr(falsch)");
            assertTrue(r != null);

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
            session.dispose();
        }
    }
}
