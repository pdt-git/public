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

package org.cs3.pl.common;

import junit.framework.TestCase;

public class UtilTest extends TestCase {

	public void testReplaceAll() {
		assertEquals(Util.replaceAll("\\\"", "\\", "\\\\"), "\\\\\"");
		assertEquals(Util.replaceAll("ab", "a", "b"), "bb");
	}
	
	public void testSplit(){
		String[] elms = Util.split("konsole -e xpce", " ");
		assertEquals(3,elms.length);
		assertEquals("konsole",elms[0]);
		assertEquals("-e",elms[1]);
		assertEquals("xpce",elms[2]);				
	}
	
	public void testSplit_empty(){
		String[] elms = Util.split("", " ");
		assertEquals(0,elms.length);					
	}
	
	public void testSplit_no_delim(){
		String[] elms = Util.split("word", " ");
		assertEquals(1,elms.length);		
		assertEquals("word",elms[0]);
	}
	
	public void testQuoteAtom(){
		String atom="something('something else')";
		assertEquals("'something(\\'something else\\')'", Util.quoteAtom(atom));
	}
	
	public void testHideStreamHandles(){
		String s="something(to($stream(hid976)))$stream(562)";
		assertEquals("something(to($stream(_)))$stream(_)", Util.hideStreamHandles(s, "$stream(_)"));
	}
	
	public void testLogicalToPhysicalOffset01() throws Throwable{
		byte[] bytes={0x30,0x0D,0x0a,0x32,0x0a,0x35};
		String data = new String(bytes);
		assertEquals(0,Util.logicalToPhysicalOffset(data,0));
		assertEquals(1,Util.logicalToPhysicalOffset(data,1));
		assertEquals(3,Util.logicalToPhysicalOffset(data,2));
		assertEquals(4,Util.logicalToPhysicalOffset(data,3));
		assertEquals(5,Util.logicalToPhysicalOffset(data,4));
		assertEquals(6,Util.logicalToPhysicalOffset(data,5));
	}
}
