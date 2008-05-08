package org.cs3.pl.prolog;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;

public class AsyncPrologSessionProxy implements AsyncPrologSession {

	private AsyncPrologSession targetSession;

	private Vector<AsyncPrologSessionListener> listeners = new Vector<AsyncPrologSessionListener>();

	private HashMap<String, String> prefs = new HashMap();

	private boolean disposed;

	private String threadAlias;

	private PrologInterface2 pif;

	protected Object targetLock = new Object();

	private boolean hasTargetSession() {

		return targetSession != null;
	}

	private Thread watchdog = new Thread() {

		public void run() {
			while (!disposed) {
				try {
					sleep(2000);
				} catch (InterruptedException e) {
					Debug.report(e);
				}
				synchronized (targetLock) {
					if (hasTargetSession() && getTargetSession().isIdle()) {
						getTargetSession().dispose();
						for (Iterator it = listeners.iterator(); it.hasNext();) {
							AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
									.next();
							getTargetSession().removeBatchListener(l);
						}
						targetSession = null;
						threadAlias = null;
					}
				}
			}
		}
	};

	private long timeout;

	private int flags;

	private AsyncPrologSession getTargetSession() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				try {
					AsyncPrologSession s = pif.getAsyncSession(this.flags);
					for (Iterator it = listeners.iterator(); it.hasNext();) {
						AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
								.next();
						s.addBatchListener(l);
					}
					s.addBatchListener(new DefaultAsyncPrologSessionListener() {

					});
					
					disposed = false;
					threadAlias = s.getProcessorThreadAlias();
					targetSession = s;

				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				}

			}
		}

		return targetSession;
	}

	public void abort() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().abort();
			}
		}

	}

	public void abort(Object monitor) throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().abort(monitor);
			}
		}
	}

	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().addBatchListener(l);
			}
			listeners.add(l);
		}
	}

	public void dispose() {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().dispose();
			}
			disposed = true;
		}
	}

	public String getProcessorThreadAlias() {
		synchronized (targetLock) {
			return threadAlias;
		}

	}

	public boolean isDisposed() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return disposed;
			}
			return getTargetSession().isDisposed();
		}
	}

	public boolean isIdle() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return true;
			}
			return getTargetSession().isIdle();
		}
	}

	public boolean isPending(Object ticket) {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return false;
			}
			return getTargetSession().isPending(ticket);
		}
	}

	public void join() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return;
			}
			getTargetSession().join();
		}
	}

	public void queryAll(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryAll(ticket, query);
		}

	}

	public void queryOnce(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryOnce(ticket, query);
		}

	}

	public void queryAll(Object ticket, String query, int flags)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryAll(ticket, query, flags);
		}

	}

	public void queryOnce(Object ticket, String query, int flags)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryOnce(ticket, query, flags);
		}

	}

	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			listeners.remove(l);
			if (hasTargetSession()) {
				getTargetSession().removeBatchListener(l);
			}
		}
	}

	


	public AsyncPrologSessionProxy(PrologInterface2 pif,int flags) {
		super();
		this.pif = pif;
		this.flags=flags;
		timeout = 1000;
		watchdog.start();
	}

	public AsyncPrologSessionProxy(PrologInterface2 pif, long timeout,int flags) {
		super();
		this.pif = pif;
		this.flags=flags;
		this.timeout = timeout;
		watchdog.start();
	}
}
