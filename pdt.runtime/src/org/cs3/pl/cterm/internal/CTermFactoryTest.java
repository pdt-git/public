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

package org.cs3.pl.cterm.internal;

import junit.framework.TestCase;

import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;

public class CTermFactoryTest extends TestCase {
	
	private CTermFactory factory;
	
	protected void setUp() throws Exception {
		this.factory = new ParserCTermFactory();
		//this.factory = new ATermFactory();
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
		CTerm term = factory.createCTerm("s�ben");		
		assertTrue("type",term instanceof CAtom);
		CAtom atom= (CAtom)term;
		assertEquals("functor value","s�ben",atom.getFunctorValue());
		assertEquals("functor image","s�ben",atom.getFunctorImage());
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
		assertTrue("type is "+term.getClass().getName(),term instanceof CString);
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
	
	public void testInteger02() throws Throwable{
		CCompound c = (CCompound) factory.createCTerm("worked(1)");
		CTerm term = c.getArgument(0);
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;		
		assertEquals(1,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testInteger03() throws Throwable{
		CCompound c = (CCompound) factory.createCTerm("worked(23)");
		CTerm term = c.getArgument(0);
		assertTrue("type",term instanceof CInteger);
		CInteger integer= (CInteger)term;		
		assertEquals(23,integer.getIntValue());
		assertEquals("arity",0,integer.getArity());	
	}
	
	public void testCompound01() throws Throwable{
		CTerm term = factory.createCTerm("aterm(annos,t)");		
		assertTrue("type: "+term.getClass().getName(),term instanceof CCompound);
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
	//'.'(label('module(op_annotator, [])'), '.'(variable_names([]), '.'(singletons([]), '.'(file_ref(93), '.'(n(1), '.'(last_n(4), '.'(position(-(2042, 2067)), '.'(functor_type(prefix), _G799))))))))
	public void testMisc00() throws Throwable{
		String input = "'.'(label('module(op_annotator, [])'), '.'(variable_names([]), '.'(singletons([]), '.'(file_ref(93), '.'(n(1), '.'(last_n(4), '.'(position(-(2042, 2067)), '.'(functor_type(prefix), _G799))))))))";
		CTerm term = factory.createCTerm("','(!,b)");		
	}
}
