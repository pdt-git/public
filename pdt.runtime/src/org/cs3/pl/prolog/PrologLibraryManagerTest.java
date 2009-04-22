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

package org.cs3.pl.prolog;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.cs3.pl.common.Util;

/*
 * each test precedes a diagram spec that you can run through dot to
 * visualize the test graph.
 * 
 */

public class PrologLibraryManagerTest extends TestCase {
	
	/*
	digraph test01 {	
		A->B;
		A->C
	}
	*/

	public void test01() throws Throwable{
		Lib a = new Lib("A","BC");
		Lib b = new Lib("B");
		Lib c = new Lib("C");
		PrologLibrary[] libs ={a,b,c};		
		String[] broken = {"A","A","","","",""};
		String[] unresolved = {"BC","C","","","",""};
		String[] resolved = {"A","AB","ABC","BC","C",""};
		
		doTest(libs,resolved,unresolved,broken);
	}
	

	/*
	digraph test02 {	
		A->B;
		A->B
	}
	*/

	public void test02() throws Throwable{
		Lib a = new Lib("A","BB");
		Lib b = new Lib("B");		
		PrologLibrary[] libs ={a,b};		
		String[] broken = {"A","","",""};
		String[] unresolved = {"B","","",""};
		String[] resolved = {"A","AB","B",""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	
	/*
	digraph test03 {	
		A->C;
		B->C
	}
	*/	
	public void test03() throws Throwable{
		Lib a = new Lib("A","C");
		Lib b = new Lib("B", "C");		
		Lib c = new Lib("C");
		PrologLibrary[] libs ={a,b,c};		
		String[] broken = {"A","AB","","","",""};
		String[] unresolved = {"C","C","","","",""};
		String[] resolved = {"A","AB","ABC","BC","C",""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	
	/*
	digraph test04 {	
		A->B;
		C->D
	}
	*/
	
	public void test04() throws Throwable{
		Lib a = new Lib("A","B");
		Lib b = new Lib("B");		
		Lib c = new Lib("C","D");
		Lib d = new Lib("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","","C","","",
				           "", "", "", ""};
		String[] unresolved = {"B","","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test05 {	
		A->C;
		A->D;
		B->C;
		B->D;
	}
	*/

	
	public void test05() throws Throwable{
		Lib a = new Lib("A","CD");
		Lib b = new Lib("B","CD");		
		Lib c = new Lib("C");
		Lib d = new Lib("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","AB","",
				           "", "", "", ""};
		String[] unresolved = {"CD","CD","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	/*
	digraph test06 {	
		A->B;
		A->C;
		B->D;
		C->D;
	}
	*/

	
	public void test06() throws Throwable{
		Lib a = new Lib("A","BC");
		Lib b = new Lib("B","D");		
		Lib c = new Lib("C","D");
		Lib d = new Lib("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","ABC","",
				           "", "", "", ""};
		String[] unresolved = {"BC","CD","D","",
				               "","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}
	
	/*
	digraph test07 {	
		A->B;
		A->C;
		D->B;
		D->C;
	}
	*/

	
	public void test07() throws Throwable{
		Lib a = new Lib("A","BC");
		Lib b = new Lib("B");		
		Lib c = new Lib("C");
		Lib d = new Lib("D","BC");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","A","","",
				           "", "D", "D", ""};
		String[] unresolved = {"BC","C","","",
				               "","B", "BC", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		

		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test08 {	
		A->B;
		B->A;
		A->C;
		C->D;
	}
	*/

	
	public void test08() throws Throwable{
		Lib a = new Lib("A","BC");
		Lib b = new Lib("B","A");		
		Lib c = new Lib("C","D");
		Lib d = new Lib("D");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","ABC","",
				           "B", "", "", ""};
		String[] unresolved = {"BC","C","D","",
				               "A","", "", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				             "BCD","CD", "D", ""};
		
		doTest(libs,resolved,unresolved,broken);
	}

	/*
	digraph test09 {	
		B->A;
		A->C;
		D->C;		
	}
	*/	
	public void test09() throws Throwable{
		Lib a = new Lib("A","C");
		Lib b = new Lib("B","A");		
		Lib c = new Lib("C");
		Lib d = new Lib("D","C");
		PrologLibrary[] libs ={a,b,c,d};		
		String[] broken = {"A","AB","","",
				           "B", "", "D", ""};
		String[] resolved = {"A","AB","ABC","ABCD",
				               "BCD","CD", "D", ""};
		String[] unresolved = {"C","C","","",
				             "A","", "C", ""};
		
		doTest(libs,resolved,unresolved,broken);
	}
	
	
	
	
	
	
	
	
	
	
	private static class Lib implements PrologLibrary{
		String id;
		Set deps;
		

		public Lib(String id, String ds){
			this.id=id;
			this.deps=new HashSet();
			for (int i = 0; i < ds.length(); i++) {
				deps.add(String.valueOf(ds.charAt(i)));
			}
		}
		public String toString() {
		
			return "Lib "+id+" -> "+Util.prettyPrint(deps.toArray());
		}
		public Lib(String id) {
			this.id=id;
			this.deps=new HashSet();
		}

		public String getId() {
			return id;
		}

		public String getPath() {
			return "path";
		}

		public String getAlias() {
			return "alias";
		}

		public Set getDependencies() {
			return deps;
		}
		public String getAttributeValue(String attr) {
			return null;
		}
		
	}
	
	

	private void doTest(PrologLibrary[] libs, String[] resolved, String[] unresolved, String[] broken) {
		Set allDeps = getAllDeps(libs);
		Map allLibs = getAllLibs(libs); 
		PrologLibraryManager mgr = new PrologLibraryManager();
		for (int i = 0; i < libs.length; i++) {
			PrologLibrary lib = libs[i];
			String msg = "after adding lib "+lib.getId();
			mgr.addLibrary(lib);
			checkResolved(msg,mgr,resolved[i],allLibs);
			checkUnresolved(msg,mgr,unresolved[i]);
			checkBroken(msg,mgr,broken[i]);	
		}
		for (int i = 0; i < libs.length; i++) {
			PrologLibrary lib = libs[i];
			mgr.removeLibrary(lib);
			String msg = "after removing lib "+lib.getId();
			checkResolved(msg,mgr,resolved[i+libs.length],allLibs);
			checkUnresolved(msg,mgr,unresolved[i+libs.length]);
			checkBroken(msg,mgr,broken[i+libs.length]);	
		}
		
	}

	private Set getAllDeps(PrologLibrary[] libs) {
		HashSet set = new HashSet();
		for (int i = 0; i < libs.length; i++) {
			set.addAll(libs[i].getDependencies());
		}
		return set;
	}

	private Map getAllLibs(PrologLibrary[] libs){
		HashMap map = new HashMap();
		for (int i = 0; i < libs.length; i++) {
			map.put(libs[i].getId(),libs[i]);
		}
		return map;
	}
	
	

	

	private void checkBroken(String msg, PrologLibraryManager mgr, String string) {
		Set exp=new HashSet();
		for (int i = 0; i < string.length(); i++) {
			exp.add(String.valueOf(string.charAt(i)));
		}
		Set act = mgr.getBrokenLibraries();
		assertEqualSet(msg+": broken libs do not match",exp,act);
	}

	private void checkUnresolved(String msg, PrologLibraryManager mgr, String string) {
		Set exp=new HashSet();
		for (int i = 0; i < string.length(); i++) {
			String key =String.valueOf(string.charAt(i)); 
			exp.add(key);
			assertNull("unresolved key "+key+" is actually resolveable",mgr.resolveLibrary(key));
		}
		Set act = mgr.getUnresolvedDependencies();
		assertEqualSet(msg+": unresolved deps do not match",exp,act);
		
	}

	private void checkResolved(String msg,PrologLibraryManager mgr, String string, Map allLibs) {
		HashMap exp=new HashMap();
		for (int i = 0; i < string.length(); i++) {
			String key = String.valueOf(string.charAt(i));
			exp.put(key,allLibs.get(key));
		}
		Set negExp = new HashSet();
		negExp.addAll(allLibs.keySet());
		negExp.removeAll(exp.keySet());
		Set keys = exp.keySet();
		for (Iterator it = keys.iterator(); it.hasNext();) {
			String key = (String) it.next();
			assertSame(msg+": "+key+" is not resolved to same lib.",exp.get(key),mgr.resolveLibrary(key));
		}
			
		for (Iterator it = negExp.iterator(); it.hasNext();) {
			String key = (String) it.next();
			assertNull(msg+": "+key+" should not be resolvable.",mgr.resolveLibrary(key));
		}
		
	}
	
	private void assertEqualSet(String msg,Set expUsers, Set actUsers) {
		String es="expected: \n"+Util.prettyPrint(expUsers);
		String as="but was: \n"+Util.prettyPrint(actUsers);			
		assertTrue(msg+": exp !>= act\n"+es+as,expUsers.containsAll(actUsers));
		assertTrue(msg+": act !>= exp\n"+es+as,actUsers.containsAll(expUsers));
	}
}
