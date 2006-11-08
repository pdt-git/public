package org.cs3.pl.tuprolog.internal;

import alice.tuprolog.Library;
import alice.tuprolog.Term;

public class ObserverLibrary extends Library {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public boolean observer_2(Term subject, Term key) {
		
		
		return true;
	}
	
	public boolean unobserver_2(Term first, Term second) {
		
		return true;
	}

	public boolean notify_2(Term subject, Term msg){
		
		return true;
	}
}
