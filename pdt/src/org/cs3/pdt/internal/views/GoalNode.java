package org.cs3.pdt.internal.views;

import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Goal;

public class GoalNode implements Goal {

	private String module;
	private CTerm term;

	public GoalNode(String contextModule, CTerm term) {
		this.module=contextModule;
		this.term = term;
	}

	public int getArity() {

		return term.getArity();
	}

	public String getName() {
		return term.getFunctorValue();
	}

	public String getModule() {
		return module;
	}

	public CTerm getTerm() {
		return term;
	}

}
