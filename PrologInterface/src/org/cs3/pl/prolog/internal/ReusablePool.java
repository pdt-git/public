/*
 */
package org.cs3.pl.prolog.internal;

import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.cs3.pl.common.Debug;

/**
 * Creation of new client sessions seems to be rather costly due to the involved
 * rpc overhead. So we try to pool unused sessions for later use.
 */
public class ReusablePool {
	HashMap pool = new HashMap();

	int maxPoolSize = 5;

	int poolSize = 0;


	public void recycle(Reusable s) {
	    s.recylce();
		addInstance(s);
	}

	protected void addInstance(Reusable s) {
		synchronized (pool) {
			
			if (poolSize >= maxPoolSize) {
				Debug.debug("maximum pool size exeeded. instance destroyed.");
				s.destroy();				
				return;
			}			
			Class clazz = s.getClass();

			LinkedList l = (LinkedList) pool.get(clazz);
			if (l == null) {
				l = new LinkedList();
				pool.put(clazz, l);
			}
			l.addLast(s);
			
			poolSize++;
			Debug.debug("new instance added to the pool. poolSize now: "+poolSize);
		}
	}

	public Reusable findInstance(Class clazz) {
		Reusable r = null;
		synchronized (pool) {
			LinkedList l = (LinkedList) pool.get(clazz);
			if (l == null) {
				Debug.debug("no reusable instance in pool");
				return null;
			}
			if (l.isEmpty()) {
				Debug.debug("no reusable instance in pool");
				return null;
			}
			poolSize--;
			r = (Reusable) l.removeFirst();

		}
		r.reuse();
		Debug.debug("instance taken from pool and reanimated. poolSize now: "+poolSize);
		return r;
	}

	public int getMaxTotalSize() {
		synchronized (pool) {
			return maxPoolSize;
		}
	}

	public void setMaxTotalSize(int size) {
		synchronized (pool) {
			this.maxPoolSize = size;
		}
	}
	
	public void clear(){
		Collection collection = pool.values();
		for (Iterator it = collection.iterator(); it.hasNext();) {
			List list = (List) it.next();
			for (Iterator it2 = list.iterator(); it2.hasNext();) {
				Reusable s = (Reusable) it2.next();
				s.destroy();
				it2.remove();
			}
			it.remove();
		}
		
	}

}