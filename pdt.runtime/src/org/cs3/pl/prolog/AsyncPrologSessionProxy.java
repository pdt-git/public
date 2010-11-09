package org.cs3.pl.prolog;

import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;

public class AsyncPrologSessionProxy implements AsyncPrologSession {

	private AsyncPrologSession targetSession;

	private Vector<AsyncPrologSessionListener> listeners = new Vector<AsyncPrologSessionListener>();

	private boolean disposed;

	private String threadAlias;

	private PrologInterface pif;

	protected Object targetLock = new Object();

	private boolean hasTargetSession() {

		return targetSession != null;
	}

	private Thread watchdog = new Thread() {

		@Override
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
						for (Iterator<AsyncPrologSessionListener> it = listeners.iterator(); it.hasNext();) {
							AsyncPrologSessionListener l = it
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

	private int flags;

	private AsyncPrologSession getTargetSession() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				try {
					AsyncPrologSession s = pif.getAsyncSession(this.flags);
					for (Iterator<AsyncPrologSessionListener> it = listeners.iterator(); it.hasNext();) {
						AsyncPrologSessionListener l = it
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

	@Override
	public void abort() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().abort();
			}
		}

	}

	@Override
	public void abort(Object monitor) throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().abort(monitor);
			}
		}
	}

	@Override
	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().addBatchListener(l);
			}
			listeners.add(l);
		}
	}

	@Override
	public void dispose() {
		synchronized (targetLock) {
			if (hasTargetSession()) {
				getTargetSession().dispose();
			}
			disposed = true;
		}
	}

	@Override
	public String getProcessorThreadAlias() {
		synchronized (targetLock) {
			return threadAlias;
		}

	}

	@Override
	public boolean isDisposed() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return disposed;
			}
			return getTargetSession().isDisposed();
		}
	}

	@Override
	public boolean isIdle() {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return true;
			}
			return getTargetSession().isIdle();
		}
	}

	@Override
	public boolean isPending(Object ticket) {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return false;
			}
			return getTargetSession().isPending(ticket);
		}
	}

	@Override
	public void join() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (!hasTargetSession()) {
				return;
			}
			getTargetSession().join();
		}
	}

	@Override
	public void queryAll(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryAll(ticket, query);
		}

	}

	@Override
	public void queryOnce(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryOnce(ticket, query);
		}

	}

	@Override
	public void queryAll(Object ticket, String query, int flags)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryAll(ticket, query, flags);
		}

	}

	@Override
	public void queryOnce(Object ticket, String query, int flags)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTargetSession().queryOnce(ticket, query, flags);
		}

	}

	@Override
	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			listeners.remove(l);
			if (hasTargetSession()) {
				getTargetSession().removeBatchListener(l);
			}
		}
	}

	


	public AsyncPrologSessionProxy(PrologInterface pif,int flags) {
		super();
		this.pif = pif;
		this.flags=flags;
		watchdog.start();
	}

	public AsyncPrologSessionProxy(PrologInterface pif, long timeout,int flags) {
		super();
		this.pif = pif;
		this.flags=flags;
		watchdog.start();
	}
}
