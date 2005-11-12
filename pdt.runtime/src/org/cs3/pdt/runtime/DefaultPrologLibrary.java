package org.cs3.pdt.runtime;

import java.util.HashSet;
import java.util.Set;

public class DefaultPrologLibrary implements PrologLibrary {

	private String id;
	private Set deps;
	private String alias;
	private String path;

	public DefaultPrologLibrary(String id, String[] deps, String alias, String path) {
		super();
		this.id = id;
		this.deps=new HashSet();
		for (int i = 0; i < deps.length; i++) {
			this.deps.add(deps[i]);
		}		
		this.alias = alias;
		this.path = path;
	}
	
	public DefaultPrologLibrary(String id, Set deps, String alias, String path) {
		super();
		this.id = id;
		this.deps = deps;
		this.alias = alias;
		this.path = path;
	}

	public String getId() {		
		return this.id;
	}

	public String getPath() {
		return this.path;
	}

	public String getAlias() {
		return this.alias;
	}

	public Set getDependencies() {
		return this.deps;
	}

}
