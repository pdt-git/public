/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.load;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class DefaultPrologLibrary implements PrologLibrary {

	private String id;
	private Set<String> deps;
	private String alias;
	private String path;
	private HashMap<String, String> attributes= new HashMap<String, String>();

	public DefaultPrologLibrary(String id, String[] deps, String alias, String path) {
		super();
		this.id = id;
		this.deps=new HashSet<String>();
		for (int i = 0; i < deps.length; i++) {
			this.deps.add(deps[i]);
		}		
		this.alias = alias;
		this.path = path;
	}
	
	public DefaultPrologLibrary(String id, Set<String> deps, String alias, String path) {
		super();
		this.id = id;
		this.deps = deps;
		this.alias = alias;
		this.path = path;
	}

	public DefaultPrologLibrary(String id, Set<String> deps, String alias, String path, Map<String, String> libAttrs) {
		this(id,deps,alias,path);
		this.attributes.putAll(libAttrs);
	}

	@Override
	public String getId() {		
		return this.id;
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public String getAlias() {
		return this.alias;
	}

	@Override
	public Set<String> getDependencies() {
		return this.deps;
	}

	@Override
	public String getAttributeValue(String attr) {
		return attributes.get(attr);
	}

}

