/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.cterm;

import org.cs3.prolog.internal.cterm.parser.ASTNode;


public class CInteger extends CTerm{
	public CInteger(ASTNode node) {
		super(node);
	}
	public int getIntValue() {		
		return Integer.parseInt(getFunctorValue());
	}

}

