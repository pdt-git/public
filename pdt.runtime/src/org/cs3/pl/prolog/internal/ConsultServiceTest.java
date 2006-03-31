/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

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
       // assertTrue(rcs.isConsulted("/fnord/fnum"));

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
       fail("how to unconsult?");
//        rcs.unconsult("/fnord/fnum");
//        //should be "No"
//
//        assertFalse(rcs.isConsulted("/fnord/fnum"));
//        String filename = System.getProperty("java.io.tmpdir") + File.separator
//                + "fnord" + File.separator + "fnum";
//        File file = new File(filename);
//        assertFalse(file.exists());

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
//            assertTrue(rcs.isConsulted("/fnord/fnum"));
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
