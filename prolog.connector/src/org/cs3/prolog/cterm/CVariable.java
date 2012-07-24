/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.cterm;

import org.cs3.prolog.internal.cterm.parser.ASTNode;


public class CVariable extends CTerm {
	public CVariable(ASTNode node) {
		super(node);
	}
	public String getVariableName() {
		return getFunctorValue();			
	}
}

