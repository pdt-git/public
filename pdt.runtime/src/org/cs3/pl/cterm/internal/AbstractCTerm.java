package org.cs3.pl.cterm.internal;

import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.parser.SimpleNode;

public abstract class AbstractCTerm implements CTerm {

	
	private CTermFactory factory;
	protected SimpleNode node;
	private String functorImage;
	private String functorValue;
	
	public CTerm getAnotation(String functor) {	
		return null;
	}
	
	public AbstractCTerm(ParserCTermFactory factory, SimpleNode node) {
		this.node=node;
		this.factory=factory;
	}

	
	public String getFunctorValue() {
		if(functorValue==null){
			functorValue=doGetFunctorValue();	
		}
		
		
		return functorValue;
	}

	protected  String doGetFunctorValue() {
		String image = getFunctorImage();
		return Util.unquoteAtom(image);
	}


	public CTermFactory getFactory() {
		
		return this.factory;
	}


	public String getFunctorImage() {
		if(functorImage==null){
			functorImage=doGetFunctorImage();
		}
		return functorImage;
	}


	protected String doGetFunctorImage() {
		return node.getImage();
	}


	public int getArity() {	
		return 0;
	}

}
