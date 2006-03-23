package org.cs3.pl.cterm.internal;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.parser.SimpleNode;

public abstract class AbstractATerm implements CTerm {

	
	private ATermFactory factory;
	protected CTerm term;
	Map annotation;
	protected boolean anotated;
	
	public CTerm getAnotation(String functor) {	
		if(annotation==null){
			annotation = new HashMap();
			if(anotated){
				processAnotations();
			}
		}
		return (CTerm) annotation.get(functor);
	}
	
	public boolean hasAnnotation(String functor) {	
		if(annotation==null){
			annotation = new HashMap();
			if(anotated){
				processAnotations();
			}
		}
		return  annotation.containsKey(functor);
	}
	
	private void processAnotations() {
		CCompound aterm =(CCompound) this.term;
		CTerm current = aterm.getArgument(0);
		while(current.getFunctorValue().equals(".")
				&& current.getArity()==2){
			CTerm term = ((CCompound)current).getArgument(0);
			processAnotation(term);
			current=((CCompound)current).getArgument(1);
		}
		
	}

	private void processAnotation(CTerm term) {
		String key = term.getFunctorValue();
		CTerm value = null;
		if( term instanceof CCompound){
			CCompound comp = (CCompound) term;
			value=comp.getArgument(0);
			if(comp.getArity()>1){
				throw new IllegalArgumentException("Illegal Annotation foramt:"+term);
			}
		}
		annotation.put(key,value);
	}

	public AbstractATerm(ATermFactory factory, CTerm aterm) {
		this.term=aterm;
		this.factory=factory;
		this.anotated = isATerm(aterm);
	}

	public static boolean isATerm(CTerm aterm) {
		return aterm.getArity()==2&& "aterm".equals(aterm.getFunctorValue());
	}

	
	
	public String getFunctorValue() {
		if(anotated){
			return unwrapOutermostATerm().getFunctorValue();
		}
		return term.getFunctorValue();
	}

	private CTerm unwrapOutermostATerm() {
		return ((CCompound)term).getArgument(1);
	}

	public static CTerm unwrapOutermostATerm( CTerm aterm) {
		return ((CCompound)aterm).getArgument(1);
	}
	

	public CTermFactory getFactory() {
		
		return this.factory;
	}


	public String getFunctorImage() {
		if(anotated){
			return unwrapOutermostATerm().getFunctorImage();
		}
		return term.getFunctorImage();
	}



	public int getArity() {	
		if(anotated){
			return unwrapOutermostATerm().getArity();
		}
		return term.getArity();

	}
	
	

}
