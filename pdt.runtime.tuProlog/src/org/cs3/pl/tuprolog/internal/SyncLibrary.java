package org.cs3.pl.tuprolog.internal;

import java.util.Hashtable;

import alice.tuprolog.Library;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;

public class SyncLibrary extends Library {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 476854551154694310L;
	/**
	 * A hashtable which stores all synchornization keys.
	 */
	private Hashtable monitors = new Hashtable();

	/**
	 * 
	 * @param key key to synchronize on.
	 * @param goal
	 * @return
	 */
	public Term with_mutex_2(Struct key, Term goal) {
		Term result = null;
		Object monitor = monitors.get(key.getName());
		if(monitor == null) {
			monitor = new Object();
			monitors.put(key.getName(), monitor);
		}
		synchronized (monitor) {
			//TODO : shall I check for a successfull query? 
			// Note: with_mutex/2 (SWI-PROLOG) does not. 
			SolveInfo info = this.getEngine().solve(goal);
			if (!info.isSuccess())
				result = null;
			else {
				try {
					result = info.getSolution();
				} catch (NoSolutionException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				} 
			}
			return result;	
		}
	}
}
