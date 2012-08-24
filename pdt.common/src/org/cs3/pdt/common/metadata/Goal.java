/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.metadata;

public class Goal extends PrologElement {

	private static final long serialVersionUID = 1L;
	private String termString;
	
	private boolean exactMatch;

	public Goal(String file, String module, String elementName, int arity, String termString) {
		this(file, 0,  module,  elementName,  arity,  termString, true);
	}
	
	public Goal(String file, String module, String elementName, int arity, String termString, boolean exactMatch) {
		this(file, 0,  module,  elementName,  arity,  termString, exactMatch);
	}

	public Goal(String file, int line, String module, String elementName, int arity, String termString) {
		this(file, line, module, elementName, arity, termString, true);
	}
	
	public Goal(String file, int line, String module, String elementName, int arity, String termString, boolean exactMatch) {
		super(file, line, module, elementName, arity);
		this.termString = termString;
		this.exactMatch = exactMatch;
	}
		

	public void setModule(String module) {
		this.contextModule=module;
	}
	
	public String getTermString() {
		if (termString != null) {
			return termString;
		} else {
			return getSignature();
		}
	}

	public boolean isExactMatch() {
		return exactMatch;
	}

}


