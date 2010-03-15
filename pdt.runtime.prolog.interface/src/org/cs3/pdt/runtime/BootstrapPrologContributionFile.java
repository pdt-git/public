package org.cs3.pdt.runtime;

import java.util.Set;


public class BootstrapPrologContributionFile extends BootstrapPrologContribution{

	private String file;
	public String getFile() {
		return file;
	}

	public BootstrapPrologContributionFile(String id, String file, Set<String> dependencies) {
		super(id, dependencies);
		this.file = file;
	}

	@Override
	public String getPrologInitStatement() {
		return "['" + file + "']";
	}
	
	

	
}
