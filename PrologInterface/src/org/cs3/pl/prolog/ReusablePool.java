/*
 */
package org.cs3.pl.prolog;

import java.util.HashMap;
import java.util.LinkedList;

import org.cs3.pl.common.Debug;

/**
 * Creation of new client sessions seems to be rather costly due to the involved
 * rpc overhead. So we try to pool unused sessions for later use.
 */
public class ReusablePool {
	HashMap pool = new HashMap();

	//HashSet softRefs = new HashSet();

	int maxPoolSize = 5;

	//int maxSoftRefSize = 5;
	
	//int gcThreshold = 4;

	int poolSize = 0;
	//protected void finalize() throws Throwable {
	//	collector.stop();
//	}
	/*
	private class Collector extends ReferenceQueue implements Runnable {
		private boolean shouldBeRunning=true;
		public synchronized void stop(){
			shouldBeRunning=false;
		}
		public void run() {
			while (getShouldBeRunning()) {
				try {
					Reference r = this.remove(1000);					
					if(r!=null){
						Reusable reusable = (Reusable) r.get();
						
						synchronized (softRefs) {
							softRefs.remove(r);
						}					
						if(reusable==null){
							Debug.warning("Now cracks a noble heart.");
						}
						addInstance(reusable);
					}
				} catch (InterruptedException e) {
					Debug.report(e);
				}
				
			}
		}
		
		private synchronized boolean getShouldBeRunning() {			
			return shouldBeRunning;
		}
		
	} */

	//Collector collector = new Collector();

	ReusablePool() {
		//new Thread(collector, "Instance Pool Collector").start();
	}

	public void recycle(Reusable s) {
		/*synchronized (softRefs) {
			
			if (softRefs.size() < maxSoftRefSize) {
				Debug.debug("creating softref");
				softRefs.add(new SoftReference(s, collector));
			} else {
				Debug
						.debug("maximum ref queue size exeeded. instance destroyed.");
				s.destroy();
			}
		}*/
		addInstance(s);
	}

	public void addInstance(Reusable s) {
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

}