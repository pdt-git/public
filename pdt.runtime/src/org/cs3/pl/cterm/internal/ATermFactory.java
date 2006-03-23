package org.cs3.pl.cterm.internal;

import org.cs3.pl.cterm.CAtom;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CFloat;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CNil;
import org.cs3.pl.cterm.CString;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.CVariable;

public class ATermFactory implements CTermFactory {

	public CTerm createCTerm(Object data) {
		if(data instanceof CTerm){
			return create((CTerm)data);
		}		
		return create(new ParserCTermFactory().createCTerm(data));
	}

	
	private  class _Atom extends AbstractATerm implements CAtom{

		public _Atom(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _String extends AbstractATerm implements CString{

		public _String(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _Nil extends AbstractATerm implements CAtom{

		public _Nil(CTerm term) {
			super(ATermFactory.this, term);
		}
		
	}
	
	private  class _Variable extends AbstractATerm implements CVariable{

		public _Variable(CTerm node) {
			super(ATermFactory.this, node);
		}

		public String getVariableId() {
			return getFunctorValue();
		}
		
	}
	
	private  class _Float extends AbstractATerm implements CFloat{

		public _Float(CTerm node) {
			super(ATermFactory.this, node);
		
		}

		public double getDoubleValue() {			
			return Double.parseDouble(getFunctorValue());
		}

		
		
	}
	private  class _Integer extends AbstractATerm implements CInteger{

		public _Integer(CTerm node) {
			super(ATermFactory.this, node);
		
		}

		public int getIntValue() {		
			return Integer.parseInt(getFunctorValue());
		}
		
	}
	
	private  class _Compound extends AbstractATerm implements CCompound{

		private CTerm[] args;

		public _Compound(CTerm node) {
			super(ATermFactory.this, node);
			args = new CTerm[getArity()]; 
		}

		public CTerm getArgument(int i) {
			if(args[i]==null){
				if(anotated){
					args[i]=create(((CCompound)((CCompound)term).getArgument(1)).getArgument(i));
				}
				else{
					args[i]=create(((CCompound)term).getArgument(i));
				}
			}
			return args[i];
		}
		
		
	}
	private  CTerm create(CTerm root) {
		CTerm wrapped;
		if(AbstractATerm.isATerm(root)){
			wrapped=AbstractATerm.unwrapOutermostATerm(root);
		}
		else{
			wrapped=root;
		}
		if(wrapped instanceof CAtom){
			return new _Atom(root);
		} 
		if(wrapped instanceof CString){
			return new _String(root);
		}
		if(wrapped instanceof CVariable){
			return new _Variable( root);
		} 
		if(wrapped instanceof CCompound){
			return new _Compound(root);
		} 
		if(wrapped instanceof CInteger){
			return new _Integer( root);
		}		
		if(wrapped instanceof CFloat){
			return new _Float(root);
		}
		if(wrapped instanceof CNil){
			return new _Nil(root);
		}
		throw new IllegalArgumentException("bad cterm type: "+root.getClass().getName());
	}

}
