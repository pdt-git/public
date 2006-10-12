package org.cs3.pl.tuprolog.internal;

import alice.tuprolog.Library;
import alice.tuprolog.Term;

public class SyncLibrary extends Library {
	
	private Object monitor = new Object();

	public boolean sync_0() {
		System.out.println("Sync was Called");
		return true;
	}
	
	public boolean with_mutex_2(Term permissions, Term goal) {
		synchronized (monitor) {
			this.getEngine().solve(goal);
			return true;
		}
	}

}
