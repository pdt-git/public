package org.cs3.pl.tuprolog.internal;

import alice.tuprolog.Term;

public class TuPrologThrowable extends Error {

	/**
	 * 
	 */
	private static final long serialVersionUID = 8770896424345716091L;
	
	private Term exception = null;
	public TuPrologThrowable(String msg, Term ex){
		super(msg);
		exception = ex;
	}
	
	
	public Term getExceptionTerm(){
		return exception;
	}

}
