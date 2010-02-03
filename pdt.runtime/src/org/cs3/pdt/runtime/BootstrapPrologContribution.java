package org.cs3.pdt.runtime;

import java.util.Set;

abstract public class BootstrapPrologContribution {

	private Set<String> dependencies;
	public Set<String> getDependencies() {
		return dependencies;
	}

	private String id;

	public String getId() {
		return id;
	}


	public BootstrapPrologContribution(String id,Set<String> dependencies) {
		this.id = id;
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
	
	public abstract String getPrologInitStatement();
	
	@Override
	public int hashCode() {
		return getId().hashCode();
	}
	
	@Override
	public boolean equals(Object obj) {
		return super.equals(((BootstrapPrologContribution)obj).getId().equals(id));
	}
}
