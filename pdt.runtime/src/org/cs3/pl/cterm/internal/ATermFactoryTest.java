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
import org.cs3.pl.cterm.CTerm;

public class ATermFactoryTest extends TestCase {
	private static ATermFactory factory = new ATermFactory();
	protected void setUp() throws Exception {
		
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
	
}
