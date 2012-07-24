/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.cterm;

import org.cs3.prolog.common.Util;
import org.cs3.prolog.internal.cterm.parser.ASTNode;

public class CTerm {
	protected ASTNode node;
	private String functorValue;
	
	public CTerm(ASTNode node) {
		this.node=node;
	}
	
	@Override
	public String toString() {
		return CTermUtil.renderTerm(this);
	}
		
	public String getFunctorValue() {
		if(functorValue==null){
			functorValue=doGetFunctorValue();	
		}
		return functorValue;
	}

	private  String doGetFunctorValue() {
		String image = getFunctorImage();
		return Util.unquoteStringOrAtom(image);
	}

	public String getFunctorImage() {
		return node.getFunctor();
	}

	public int getArity() {	
		return 0;
	}

}

