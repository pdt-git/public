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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;



/**
 * TODO: add docs.
 * @author lukas
 *
 */
public class PrologLibraryManager {
	/**
	 * maps ids to libraries
	 */
	private HashMap<String, PrologLibrary> libraries = new HashMap<String, PrologLibrary>();

	
	/**
	 * Contains unresolved dependencies
	 * i.e., ids on which other libs depend, but for which
	 * no library is registered.
	 */
	private HashSet<String> unresolvedDependencies = new HashSet<String>();
	
	/**
	 * Contains ids of libs with broken dependencies.
	 * A library is broken, if it has unresolved dependencies
	 * or depends on a broken library.
	 */
	private HashSet<String> brokenLibraries = new HashSet<String>();
	
	public void  check(){
		Set<String> done = new HashSet<String>();
		
		brokenLibraries.clear();
		unresolvedDependencies.clear();		
		Set<String> todo = libraries.keySet();
		for (Iterator<String> it = todo.iterator(); it.hasNext();) {
			String key = it.next();
			check(key, done);
		}
	}
	
	private void check(String key,Set<String> done) {
		if(done.contains(key)){
			return;
		}
		PrologLibrary lib = resolveLibrary(key);
		if(lib==null){
			unresolvedDependencies.add(key);
			return;
		}
		Set<String> dependencies = lib.getDependencies();
		done.add(key);
		for (Iterator<String> it = dependencies.iterator(); it.hasNext();) {
			String dep = it.next();
			check(dep,done);
			if(brokenLibraries.contains(dep)
			||unresolvedDependencies.contains(dep)){
				brokenLibraries.add(key);
			}
		}
	}

	public void addLibrary(PrologLibrary nlib){
		libraries.put(nlib.getId(),nlib);
		check();
	}
	
	public void removeLibrary(PrologLibrary lib){
		libraries.remove(lib.getId());
		check();
	}
	
	public Set<String> getUnresolvedDependencies(){
		return Collections.unmodifiableSet(unresolvedDependencies);
	}

	public Set<String> getBrokenLibraries(){
		return brokenLibraries;
	}
	
	public PrologLibrary resolveLibrary(String id){
		return libraries.get(id);
	}
}
