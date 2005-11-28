package org.cs3.pdt.runtime;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;
import java.util.Stack;


/**
 * TODO: this should be an interface.
 * TODO: add docs.
 * @author lukas
 *
 */
public class PrologLibraryManager {
	/**
	 * maps ids to libraries
	 */
	private HashMap libraries = new HashMap();

	
	/**
	 * Contains unresolved dependencies
	 * i.e., ids on which other libs depend, but for which
	 * no library is registered.
	 */
	private HashSet unresolvedDependencies = new HashSet();
	
	/**
	 * Contains ids of libs with broken dependencies.
	 * A library is broken, if it has unresolved dependencies
	 * or depends on a broken library.
	 */
	private HashSet brokenLibraries = new HashSet();
	
	public void  check(){
		Set done = new HashSet();
		
		brokenLibraries.clear();
		unresolvedDependencies.clear();		
		Set todo = libraries.keySet();
		for (Iterator it = todo.iterator(); it.hasNext();) {
			String key = (String) it.next();
			check(key, done);
			
		}
	}
	
	private void check(String key,Set done) {
		if(done.contains(key)){
			return;
		}
		PrologLibrary lib = resolveLibrary(key);
		if(lib==null){
			unresolvedDependencies.add(key);
			return;
		}
		Set dependencies = lib.getDependencies();
		done.add(key);
		for (Iterator it = dependencies.iterator(); it.hasNext();) {
			String dep = (String) it.next();
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
	
		
	public Set getUnresolvedDependencies(){
		return Collections.unmodifiableSet(unresolvedDependencies);
	}

	public Set getBrokenLibraries(){
		return brokenLibraries;
	}
	
	public PrologLibrary resolveLibrary(String id){
		return (PrologLibrary) libraries.get(id);
	}
}
