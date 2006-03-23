package org.cs3.pl.cterm.internal;

import junit.framework.TestCase;

import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;

public class ATermFactoryTest extends TestCase {
	private ATermFactory factory;

	protected void setUp() throws Exception {
		this.factory = new ATermFactory();
		super.setUp();
	}
	
	public void testSingleAnno00() throws Throwable{
		CTerm term = factory.createCTerm("aterm('.'(anno,[]),hola)");		
		assertTrue("type is "+term.getClass().getName(),term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","hola",atom.getFunctorValue());
		assertEquals("functor image","hola",atom.getFunctorImage());
		assertEquals("arity",0,atom.getArity());
		assertTrue("has no anno",atom.hasAnnotation("anno"));
	}
}
