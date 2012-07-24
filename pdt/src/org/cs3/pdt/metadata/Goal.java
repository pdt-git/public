/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.metadata;

public class Goal extends PrologElement {

	private static final long serialVersionUID = 1L;
	private String termString;

	public Goal(String file, String module, String elementName, int arity, String termString) {
		this(file, 0,  module,  elementName,  arity,  termString);
	}

	public Goal(String file, int line, String module, String elementName, int arity, String termString) {
		super(file, line, module, elementName, arity);
		this.termString = termString;
	}
		

	public void setModule(String module) {
		this.contextModule=module;
	}
	
	public String getTermString() {
		return termString;
	}

}

