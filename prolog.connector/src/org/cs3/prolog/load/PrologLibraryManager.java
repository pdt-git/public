/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.load;

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

