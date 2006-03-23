package org.cs3.pl.metadata;

import org.cs3.pl.cterm.CTerm;

/**@deprecated*/
public class GoalData extends PrologElementData implements Goal {

	private static final long serialVersionUID = 1L;

	public GoalData(String module, String elementName, int arity) {
		super(module, elementName, arity);	
	}

	public CTerm getTerm() {
		// TODO Auto-generated method stub
		return null;
	}

}
