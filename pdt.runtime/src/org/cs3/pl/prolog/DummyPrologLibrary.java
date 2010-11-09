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
	@Override
	public String toString() {
	
		return "Lib "+id+" -> "+Util.prettyPrint(deps.toArray());
	}
	public DummyPrologLibrary(String id) {
		this.id=id;
		this.deps=new HashSet<String>();
	}

	@Override
	public String getId() {
		return id;
	}

	@Override
	public String getPath() {
		return "path";
	}

	@Override
	public String getAlias() {
		return "alias";
	}

	@Override
	public Set<String> getDependencies() {
		return deps;
	}
	@Override
	public String getAttributeValue(String attr) {
		return null;
	}
	
}