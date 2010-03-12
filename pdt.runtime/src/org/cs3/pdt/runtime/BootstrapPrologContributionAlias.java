package org.cs3.pdt.runtime;

import java.util.Set;

import org.cs3.pl.prolog.BootstrapPrologContribution;

public class BootstrapPrologContributionAlias extends
		BootstrapPrologContribution {

	private String alias;
	private String path;

	public BootstrapPrologContributionAlias(String id, String alias, String path, Set<String> dependencies) {
		super(id, dependencies);
		this.alias = alias;
		this.path = path;
	}

	@Override
	public String getPrologInitStatement() {
		return "assertz(user:file_search_path(" + alias + ", '"+path+"'))";
	}

}
