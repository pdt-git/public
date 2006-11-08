package org.cs3.pl.tuprolog.internal;

import alice.tuprolog.Term;

public class RecordEntry {
	private int ref = 0;
	private Term term = null;
	
	public RecordEntry(Term term){
		this.term = term;
		ref = (int) ( Math.random() * 10000 );		
	}

	public Term getTerm() {
		return term;
	}

	public void setTerm(Term term) {
		this.term = term;
	}

	public int getRef() {
		return ref;
	}	
}
