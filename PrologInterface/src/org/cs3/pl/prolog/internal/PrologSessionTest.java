package org.cs3.pl.prolog.internal;

import java.io.File;
import java.io.FileWriter;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import junit.framework.TestCase;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologSession;

/**
 * @author terra
 */
public class PrologSessionTest extends TestCase {
	private PrologInterface pif;

    /* (non-Javadoc)
     * @see junit.framework.TestCase#setUp()
     */
    protected void setUp() throws Exception {
              
     
      //pif=PrologInterfaceFactory.newInstance("org.cs3.pl.prolog.internal.xml.Factory").create();
      pif=PrologInterfaceFactory.newInstance().create();
      pif.start();
    }
    
    /* (non-Javadoc)
     * @see junit.framework.TestCase#tearDown()
     */
    protected void tearDown() throws Exception {
        pif.stop();
    }
	public void testCreation(){
		PrologSession ss = pif.getSession();
		
		assertNotNull(ss);
		ss.dispose();
	}
	
	public void testQueries() throws PrologException{
		PrologSession ss = pif.getSession();
		
		assertNotNull(ss.query("assert(a(b))"));
		assertNotNull(ss.query("assert(a(v))"));
		assertNotNull(ss.query("a(v)"));
		assertNull(ss.query("a(j)"));
		
		Map tab = ss.query("a(X)");
		
		assertTrue(tab.containsKey("X"));
		
		String str = (String) tab.get("X");
		
		assertTrue(str.equals("v") || str.equals("b"));
		assertNotNull(ss.next());
		assertNull(ss.next());
		
		ss.dispose();
	}
	
	public void testConsult() throws Exception{
		File tmpdir = new File (System.getProperty("java.io.tmpdir"));
		File tmpfile = new File(tmpdir, "test.pl");
		
		Debug.debug("Tempfile created in " + System.getProperty("java.io.tmpdir"));
		
		tmpfile.deleteOnExit();
		
		FileWriter fw = new FileWriter(tmpfile);
		fw.write("v(x).\n");
		fw.flush();
		fw.close();
		
		PrologSession ss = pif.getSession();
		
		assertTrue(ss.consult(tmpfile.toString()));
		assertNotNull(ss.query("v(x)"));
	}
	
	public void testDispose() throws PrologException  {
		PrologSession ss = pif.getSession();
		
		ss.dispose();
		
		try {
			ss.query("a(X).");
		} catch (IllegalStateException e){
			return;
		}
		
		fail("Exception thrown on disposed object");
	}
	
	public void testMultipleQuery() throws PrologException {
		PrologSession server = pif.getSession(); 
		
        //ClientConnection connection= new ClientConnectionStub();
        Map r = server.query("assert(wahr(wahrheit))");
        assertNotNull("result should not be null",r);
        assertTrue("result should be empty",r.isEmpty());
        assertNull("Threre should be no further solution",server.next());
        r=server.query("assert(wahr(wahr(wahrheit)))");
        assertNotNull("result should not be null",r);
        assertTrue("result should be empty",r.isEmpty());
        assertNull("Threre should be no further solution",server.next());
        r=server.query("wahr(A)");
        assertNotNull("result should not be null",r);
        assertTrue("result should be not empty", !r.isEmpty());
        
        Vector v= new Vector(); 
        while(r!=null){
            v.add(r.get("A").toString());
            r=server.next();
        }
        assertTrue("there should be exactly two solutions!",v.size()==2);
        assertTrue(v.contains("wahrheit"));
        assertTrue(v.contains("wahr(wahrheit)"));
    }
	
	/**
     * this one fails if the pif impl does not support lists
     */
    public void testList() {
       PrologSession s = pif.getSession();
       Map map = s.queryOnce("A=[1,2,3,[[a,b,['{}']]],[b,c]]");
       
       Object A = map.get("A");
       assertEquals("[1, 2, 3, [[a, b, [{}]]], [b, c]]",A.toString());
       assertTrue(A instanceof List);
       List l = (List) A;
       assertEquals("1",(String)l.get(0));
       assertEquals("2",(String)l.get(1));
       assertEquals("3",(String)l.get(2));
       assertEquals(5,l.size());
       A=l.get(3);
       assertTrue(A instanceof List);
       List m=(List) A;
       assertEquals(1, m.size());
       assertTrue(m.get(0) instanceof List);
       m=(List) m.get(0);
       assertEquals(3,m.size());
       assertEquals("a",(String)m.get(0));
       assertEquals("b",(String)m.get(1));
       assertTrue(m.get(2) instanceof List);
       m=(List) m.get(2);
       assertEquals(1,m.size());
       assertEquals("{}",(String)m.get(0));
       assertTrue(l.get(4) instanceof List);
       m=(List) l.get(4);
       assertEquals(2,m.size());
       assertEquals("b",(String)m.get(0));
       assertEquals("c",(String)m.get(1));
       
    }
}
