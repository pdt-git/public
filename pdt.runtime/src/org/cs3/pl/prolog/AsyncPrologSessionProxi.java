package org.cs3.pl.prolog;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Vector;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Option;

public class AsyncPrologSessionProxi implements AsyncPrologSession {

	private WeakReference<AsyncPrologSession> target;

	private Vector<AsyncPrologSessionListener> listeners = new Vector<AsyncPrologSessionListener>();

	private HashMap<String, String> prefs = new HashMap();

	private boolean disposed;

	private String threadAlias;

	private PrologInterface2 pif;

	protected Object targetLock = new Object();

	private boolean hasTarget() {

		return target != null && target.get() != null;
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
					if (hasTarget() && getTarget().isIdle()) {
						getTarget().dispose();
						for (Iterator it = listeners.iterator(); it
								.hasNext();) {
							AsyncPrologSessionListener l = (AsyncPrologSessionListener) it.next();
							getTarget().removeBatchListener(l);
						}
						target = null;
						threadAlias=null;
					}
				}
			}
		}
	};

	private long timeout;

	private AsyncPrologSession getTarget() {
		synchronized (targetLock) {
			if (!hasTarget()) {
				try {
					AsyncPrologSession s = pif.getAsyncSession();
					for (Iterator it = listeners.iterator(); it.hasNext();) {
						AsyncPrologSessionListener l = (AsyncPrologSessionListener) it
								.next();
						s.addBatchListener(l);
					}
					s.addBatchListener(new DefaultAsyncPrologSessionListener() {

					});
					for (Iterator it = prefs.keySet().iterator(); it.hasNext();) {
						String key = (String) it.next();
						String value = prefs.get(key);
						s.setPreferenceValue(key, value);
					}
					s.reconfigure();
					disposed = false;
					threadAlias = s.getProcessorThreadAlias();
					target = new WeakReference<AsyncPrologSession>(s);

				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				}

			}
		}

		return target.get();
	}

	public void abort() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().abort();
			}
		}

	}

	public void abort(Object monitor) throws PrologInterfaceException {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().abort(monitor);
			}
		}
	}

	public void addBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().addBatchListener(l);
			}
			listeners.add(l);
		}
	}

	public void dispose() {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().dispose();
			}
			disposed = true;
		}
	}

	public Object getLastAbortTicket() {
		synchronized (targetLock) {
			if (hasTarget()) {
				return getTarget().getLastAbortTicket();
			}
			return null;
		}
	}

	public String getProcessorThreadAlias() {
		synchronized (targetLock) {
			return threadAlias;
		}

	}

	public boolean isDisposed() {
		synchronized (targetLock) {
			if (!hasTarget()) {
				return disposed;
			}
			return getTarget().isDisposed();
		}
	}

	public boolean isIdle() {
		synchronized (targetLock) {
			if (!hasTarget()) {
				return true;
			}
			return getTarget().isIdle();
		}
	}

	public boolean isPending(Object ticket) {
		synchronized (targetLock) {
			if (!hasTarget()) {
				return false;
			}
			return getTarget().isPending(ticket);
		}
	}

	public void join() throws PrologInterfaceException {
		synchronized (targetLock) {
			if (!hasTarget()) {
				return;
			}
			getTarget().join();
		}
	}

	public void queryAll(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTarget().queryAll(ticket, query);
		}

	}

	public void queryOnce(Object ticket, String query)
			throws PrologInterfaceException {
		synchronized (targetLock) {
			getTarget().queryOnce(ticket, query);
		}

	}

	public void removeBatchListener(AsyncPrologSessionListener l) {
		synchronized (targetLock) {
			listeners.remove(l);
			if (hasTarget()) {
				getTarget().removeBatchListener(l);
			}
		}
	}

	public Option[] getOptions() {

		synchronized (targetLock) {
			return getTarget().getOptions();
		}
	}

	public String getPreferenceValue(String id, String string) {
		synchronized (targetLock) {
			if (!hasTarget()) {
				return prefs.get(id);
			}
			return getTarget().getPreferenceValue(id, string);
		}
	}

	public void reconfigure() {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().reconfigure();
			}
		}
	}

	public void setPreferenceValue(String id, String value) {
		synchronized (targetLock) {
			if (hasTarget()) {
				getTarget().setPreferenceValue(id, value);
			}
			prefs.put(id, value);
		}
	}

	public AsyncPrologSessionProxi(PrologInterface2 pif) {
		super();
		this.pif = pif;
		timeout=1000;
		watchdog.start();
	}

	public AsyncPrologSessionProxi(PrologInterface2 pif,long timeout) {
		super();
		this.pif = pif;
		this.timeout=timeout;
		watchdog.start();
	}
}
