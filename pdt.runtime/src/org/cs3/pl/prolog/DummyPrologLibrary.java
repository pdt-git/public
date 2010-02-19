/**
 * 
 */
package org.cs3.pl.prolog;

import java.util.HashSet;
import java.util.Set;

import org.cs3.pl.common.Util;

class DummyPrologLibrary implements PrologLibrary{
	String id;
	Set<String> deps;
	

	public DummyPrologLibrary(String id, String dependenciess){
		this.id=id;
		this.deps=new HashSet<String>();
		for (int i = 0; i < dependenciess.length(); i++) {
			deps.add(String.valueOf(dependenciess.charAt(i)));
		}
	}
	public String toString() {
	
		return "Lib "+id+" -> "+Util.prettyPrint(deps.toArray());
	}
	public DummyPrologLibrary(String id) {
		this.id=id;
		this.deps=new HashSet<String>();
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

	public Set<String> getDependencies() {
		return deps;
	}
	public String getAttributeValue(String attr) {
		return null;
	}
	
}