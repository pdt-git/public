package org.cs3.pdt.runtime;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

public class PrologLibraryManager {
	/**
	 * maps ids to libraries
	 */
	private HashMap libraries = new HashMap();

	/**
	 * maps  ids to lists of libraries that depend on that
	 * ids
	 */
	private HashMap inverseDependencies = new HashMap();
	
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
	
	
	public boolean addLibrary(PrologLibrary nlib){
		
		Stack todo = new Stack();
		todo.push(nlib);
		
		while(!todo.isEmpty()){
			PrologLibrary lib= (PrologLibrary) todo.pop();
			List deps = lib.getDependencies();
			boolean ok = true;
			for (Iterator it = deps.iterator(); it.hasNext();) {
				String dep = (String) it.next();
				addInverseDependency(dep,lib);
				if(!libraries.containsKey(dep)){
					unresolvedDependencies.add(dep);
					ok=false;
				}
			}
			
			if(ok&&unresolvedDependencies.contains(lib.getId())){
				unresolvedDependencies.remove(lib.getId());
				todo.addAll((Set) inverseDependencies.get(lib.getId()));
			}			
		}
		return libraries.containsKey(nlib.getId());
	}
	
	public void removeLibrary(PrologLibrary lib){
		List deps = lib.getDependencies();
		for (Iterator it = deps.iterator(); it.hasNext();) {
			String dep = (String) it.next();
			removeInverseDependency(dep,lib);
			if(!hasInverseDependency(dep)){
				unresolvedDependencies.remove(dep);
			}
		}
		libraries.remove(lib.getId());
		brokenLibraries.remove(lib.getId());
		Stack todo = new Stack();
		List users = getInversedDependencies(lib.getId());
		todo.addAll(users);
		while(!todo.isEmpty()){
			PrologLibrary top = (PrologLibrary) todo.pop();
			//if top is already marked as broken, then so are it's users
			//otherwise: traverse.
			if(!brokenLibraries.contains(top)){
				brokenLibraries.add(top);
				todo.addAll(getInversedDependencies(top.getId()));
			}
		}
	}
	
	public List getInversedDependencies(String id) {
		List l = (List) inverseDependencies.get(id);
		return l==null?new Vector():l;
	}

	public boolean hasInverseDependency(String provider) {
		Set users = (Set) inverseDependencies.get(provider);
		return users==null||users.isEmpty();
	}

	private void addInverseDependency(String provider, PrologLibrary user) {
		Set users = (Set) inverseDependencies.get(provider);
		if(users==null){
			users=new HashSet();
			inverseDependencies.put(provider,users);
		}
		users.add(user);
		
	}

	private void removeInverseDependency(String provider, PrologLibrary user) {
		Set users = (Set) inverseDependencies.get(provider);
		if(users==null){
			return;
		}
		users.remove(user);
		
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
