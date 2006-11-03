package org.cs3.pl.tuprolog.internal;

import java.util.Hashtable;

import alice.tuprolog.Library;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;

public class OperatorsExtLibrary extends Library {
	Hashtable hsh = new Hashtable();
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public boolean structEq_2(Term x,Term y){
		/*
		 * Extracts real Terms from TuProlog bindings.
		 * => a(b,c) compound is passed as X_e / a(b,c).
		 */
		Term first_term = x.getTerm();
		Term second_term = y.getTerm();
		
		if (first_term.isEqual(second_term))
			return true;

		if (first_term.isVar() && second_term.isVar() )
			return true;
		
		if(first_term.isCompound() && second_term.isCompound() ) {
			Struct frst =((Struct)first_term);
			Struct scnd =((Struct)second_term);
			/*
			 * checks the functor name
			 */
			if ( frst.getName() != scnd.getName() )
				return false;
			
			int arity = frst.getArity();
			/*
			 * checks the arity number
			 */
			if ( arity == scnd.getArity()){
			
				for (int i = 0; i < arity ; i++) {
					Term arg_1 = frst.getArg(i);
					Term arg_2 = scnd.getArg(i);

					/*
					 * Calls recursivelly structural equality between subterms.
					 */
					if (!structEq_2(arg_1 , arg_2 ))
						return false;
					/*
					 * Hashtable which stores the position of occurance for each 
					 * Var subterm.
					 */
					if (arg_1.isVar())
						if ( hsh.containsKey(arg_1) || hsh.containsValue(arg_2)){
							if ( ((Term)hsh.get(arg_1)) != arg_2)
								return false;
						} else{
							hsh.put(arg_1, arg_2);
						}
				}
				
				/*
				 * All subterms are structurally equal.
				 */
				return true;				
			}
			
			/*
			 * differenet arities.
			 */
			return false;
		}
		
		/*
		 *  The rest are false
		 */
		return false;
	}
	
	public String getTheory(){
		 return ":- op(700, xfx, '=@='). \n" +
		 		":- op(700, xfx, '\\=@='). \n" +
		 		"'=@='(X,Y):- structEq(X,Y).\n" +
		 		"'\\=@='(X,Y):- not structEq(X,Y).\n" +
		 		"check(X):-nonvar(X), assert(X).";
	}
}
