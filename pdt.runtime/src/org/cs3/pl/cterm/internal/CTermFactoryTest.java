package org.cs3.pl.cterm.internal;

import junit.framework.TestCase;

import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;

public class CTermFactoryTest extends TestCase {
	private ParserCTermFactory factory;
	protected void setUp() throws Exception {
		this.factory = new ParserCTermFactory();
		super.setUp();
	}
	public void testUnquotedAtom00() throws Throwable {
		CTerm term = factory.createCTerm("hola");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","hola",atom.getFunctorValue());
		assertEquals("functor image","hola",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	public void testUnquotedAtom01() throws Throwable {
		CTerm term = factory.createCTerm("hol_a");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","hol_a",atom.getFunctorValue());
		assertEquals("functor image","hol_a",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testUnquotedAtom02() throws Throwable {
		CTerm term = factory.createCTerm("holT0_a");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","holT0_a",atom.getFunctorValue());
		assertEquals("functor image","holT0_a",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testUnquotedAtom03() throws Throwable {
		try{
			CTerm term = factory.createCTerm("23skido?");
			fail("atoms are not allowed to start with digits. there should be an exception!");
		}catch(Exception pe){			
			return;
		}
				
	}
	public void testUnquotedAtom04() throws Throwable {
		CTerm term = factory.createCTerm("söben");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","söben",atom.getFunctorValue());
		assertEquals("functor image","söben",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());		
	}
	
	public void testQuotedAtom01() throws Throwable{
		CTerm term = factory.createCTerm("'Ecce, Corinna venit.'");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","Ecce, Corinna venit.",atom.getFunctorValue());
		assertEquals("functor image","'Ecce, Corinna venit.'",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());
	}
	
	public void testQuotedAtom02() throws Throwable{
		CTerm term = factory.createCTerm("'Ecce, \\'Corinna\\' venit.'");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor image","'Ecce, \\'Corinna\\' venit.'",atom.getFunctorImage());
		assertEquals("functor value","Ecce, 'Corinna' venit.",atom.getFunctorValue());
		
		assertEquals("arity",0,atom.getArity());
	}
	
	
	public void testQuotedStromg01() throws Throwable{
		CTerm term = factory.createCTerm("\"Ecce, \\\"Corinna\\\" venit.\"");		
		assertTrue("type is "+term.getClass().getCanonicalName(),term instanceof CString);
		CString string= (CString)term;
		assertEquals("functor image","\"Ecce, \\\"Corinna\\\" venit.\"",string.getFunctorImage());
		assertEquals("functor value","Ecce, \"Corinna\" venit.",string.getFunctorValue());
		
		assertEquals("arity",0,string.getArity());
	}
	
	
	public void testInteger01() throws Throwable{
		CTerm term = factory.createCTerm("42");		
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;
		assertEquals("functor image","42",integer.getFunctorImage());
		assertEquals("functor value","42",integer.getFunctorValue());
		assertEquals(42,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testCompound01() throws Throwable{
		CTerm term = factory.createCTerm("aterm(annos,t)");		
		assertTrue("type",term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value","aterm",aterm.getFunctorValue());
		assertEquals("functor image","aterm",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","annos",annos.getFunctorValue());
		assertEquals("annos functor image","annos",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","t",t.getFunctorValue());
		assertEquals("t functor image","t",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
	
	public void testCompound02() throws Throwable{
		CTerm term = factory.createCTerm(";(a,b)");		
		assertTrue("type",term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value",";",aterm.getFunctorValue());
		assertEquals("functor image",";",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","a",annos.getFunctorValue());
		assertEquals("annos functor image","a",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","b",t.getFunctorValue());
		assertEquals("t functor image","b",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
	public void testCut00() throws Throwable{
		CTerm term = factory.createCTerm("','(!,b)");		
		assertTrue("type",term instanceof CCompound);
		CCompound aterm= (CCompound)term;
		assertEquals("functor value",",",aterm.getFunctorValue());
		assertEquals("functor image","','",aterm.getFunctorImage());
		assertEquals("arity",2,aterm.getArity());		
		assertNotNull("arg0",aterm.getArgument(0));
		assertNotNull("arg1",aterm.getArgument(1));
		CTerm annos = aterm.getArgument(0);
		assertTrue("type annos",annos instanceof CAtom);
		assertEquals("annos functor value","!",annos.getFunctorValue());
		assertEquals("annos functor image","!",annos.getFunctorImage());
		assertEquals("annos arity",0,annos.getArity());
		
		CTerm t = aterm.getArgument(1);
		assertTrue("type t",t instanceof CAtom);
		assertEquals("t functor value","b",t.getFunctorValue());
		assertEquals("t functor image","b",t.getFunctorImage());
		assertEquals("t arity",0,t.getArity());
		
	}
}
