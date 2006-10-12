package org.cs3.pl.tuprolog.internal;

import com.sun.org.apache.xalan.internal.xsltc.runtime.Hashtable;

import alice.tuprolog.Library;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;

public class SyncLibrary extends Library {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 476854551154694310L;
	private Hashtable monitors = new Hashtable();

	public boolean sync_0() {
		System.out.println("Sync was Called");
		return true;
	}
	/**
	 * 
	 * @param permissions key to synchronize on.
	 * @param goal
	 * @return
	 */
	public boolean with_mutex_2(Struct permissions, Term goal) {
		Object monitor = monitors.get(permissions.getName());
		if(monitor == null) {
			monitor = new Object();
			monitors.put(permissions.getName(), monitor);
		}
		synchronized (monitor) {
			this.getEngine().solve(goal);
			return true;
		}
	}
}
