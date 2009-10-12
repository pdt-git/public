package org.cs3.pdt.runtime;

import java.util.Set;

public class BootstrapPrologContribution {

	private Set<String> dependencies;
	public Set<String> getDependencies() {
		return dependencies;
	}

	private String file;
	private String id;

	public String getId() {
		return id;
	}

	public String getFile() {
		return file;
	}

	public BootstrapPrologContribution(String id,String file,Set<String> dependencies) {
		this.id = id;
		this.file = file;
		this.dependencies = dependencies;
	}

	@Override
	public String toString() {
		String contribString = "bootstrap contribution " + id;
		if(dependencies == null || dependencies.size() == 0){
			return contribString;
		}
		return contribString + " depends on [" + dependencies+ "]";
	}
}
