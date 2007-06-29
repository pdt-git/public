/**
 * 
 */
package org.cs3.pl.prolog.internal.pifcom;

import java.util.HashSet;
import java.util.TreeMap;

import org.cs3.pl.common.Debug;

class JackTheProcessRipper extends Thread {

	static JackTheProcessRipper instance;

	/**
	 * the heap contains processes that are scheduled for termination. The times
	 * at which they should be killed is used as Keys. (Long)
	 */
	private TreeMap heap = new TreeMap();

	/**
	 * processes is a set of all processes that were started using the enclosing
	 * strategy. When the vm is shutting down, the ripper will make sure that
	 * the heap is empty AND the processes set are empty. It will not allow the
	 * vm to exit before this condition holds. It will not allow the creation of
	 * another process once the shutdown has begun.
	 */
	private HashSet processes = new HashSet();

	private boolean shuttingDown = false;

	public static JackTheProcessRipper getInstance() {
		if (instance == null) {
			instance = new JackTheProcessRipper();
		}
		return instance;
	}

	private JackTheProcessRipper() {
		super("Jack the Process Ripper");
		setDaemon(false);
		Runtime.getRuntime().addShutdownHook(
				new Thread("Jack The Process Ripper Shutdown Hook") {
					public void run() {
						shuttingDown = true;
					}
				});
		start();
	}

	public void registerProcess(ProcessWrapper p) {
		if (shuttingDown) {
			throw new IllegalStateException(
					"you cannot register processes during shutdown");
		}
		synchronized (processes) {
			processes.add(p);
		}
	}

	public void run() {
		ProcessWrapper process = null;
		// Runtime.getRuntime().addShutdownHook(this);
		// FIXME: what to do on vm shutdown?
		while (!(shuttingDown && processes.isEmpty() && heap.isEmpty())) {
			process = dequeue();
			try {
				process.destroy();
				processes.remove(process);
			} catch (Throwable t) {
				Debug.report(t);
			}

		}

	}

	private ProcessWrapper dequeue() {
		synchronized (heap) {
			while (heap.isEmpty()) {
				try {
					heap.wait(5000);
				} catch (InterruptedException e) {
					Debug.report(e);
				}
			}
			Long next = (Long) heap.firstKey();
			if (System.currentTimeMillis() - next.longValue() > 20) {
				try {
					Thread.sleep(20 + next.longValue()
							- System.currentTimeMillis());
				} catch (InterruptedException e) {
					Debug.report(e);
				}
			}
			return (ProcessWrapper) heap.remove(next);
		}
	}

	public void enqueue(ProcessWrapper p, long timeout) {
		synchronized (heap) {
			long due = System.currentTimeMillis() + timeout;
			Long key = null;
			while (heap.containsKey(key = new Long(due))) {
				due++;
			}
			heap.put(key, p);
			heap.notifyAll();
		}
	}
}