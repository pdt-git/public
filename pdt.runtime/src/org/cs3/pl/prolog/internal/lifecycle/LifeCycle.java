package org.cs3.pl.prolog.internal.lifecycle;

import java.util.HashMap;
import java.util.HashSet;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.LifeCycleHook3;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;

public abstract class LifeCycle {

	private class DispatcherThread extends Thread {
		private boolean shouldBeRunning = true;
		private BlockingQueue<WorkRunnable> queue = new LinkedBlockingQueue<WorkRunnable>();

		public DispatcherThread(String name) {
			super(name);
		}

		private synchronized void enqueue(WorkRunnable r) {
			String name = runnableName(r);
			Debug.debug("enqueue on " + getName() + ": " + name);
			queue.add(r);
		}

		private String runnableName(WorkRunnable r) {
			String name = r instanceof NamedWorkRunnable ? ((NamedWorkRunnable) r)
					.getName()
					: "unnamed work";
			return name;
		}

		@Override
		public void run() {
			while (shouldBeRunning) {
				try {
					WorkRunnable r = queue.take();
					String name = runnableName(r);
					Debug.debug("processing: " + name);
					r.run();
				} catch (Throwable t) {

					Debug.report(t);
					Debug.warning("clearing queue");
					queue.clear();
					error(t);
				}
			}
		}

		public void stopRunning() {
			queue.clear();
			queue.add(new NamedWorkRunnable("shouldBeRunning=false") {

				@Override
				public void run() throws PrologInterfaceException {
					shouldBeRunning = false;

				}

			});
			try {
				join();
			} catch (InterruptedException e) {

			}
		}
	}

	private State state = null;

	private HashMap<String, LifeCycleHookWrapper> hooks = new HashMap<String, LifeCycleHookWrapper>();

	private DispatcherThread workThread;
	private DispatcherThread transitionThread;

	public LifeCycle(String name) {

		setState(new DownState(this));
		workThread = new DispatcherThread("PIF " + name + " Work");
		workThread.start();
		transitionThread = new DispatcherThread("PIF " + name + " Transitions");
		transitionThread.start();
	}

	public synchronized void dispose() {
		workThread.stopRunning();
		transitionThread.stopRunning();
		hooks.clear();
		hooks = null;

		workThread = null;
		transitionThread = null;
	}

	public synchronized void waitUntilUp() throws InterruptedException,
			PrologInterfaceException {
		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"cannot call this from transition or work queue.");
		}

		while (getError() == null && !isUp()) {
			this.wait();
			if (isDown()) {
				start();
			}
		}
		if (getError() != null) {
			throw new PrologInterfaceException(getError());
		}
	}

	public synchronized void waitUntilDown(boolean ignoreErrors) throws InterruptedException,
			PrologInterfaceException {

		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"cannot call this from transition or work queue.");
		}
		while ((ignoreErrors||getError() == null) && !isDown()) {
			this.wait();
			if (isUp()) {
				stop();
			}

		}
		if (null != getError()) {
			throw new PrologInterfaceException(getError());
		}
	}

	

	public void waitUntilError() throws InterruptedException {
		if (Thread.currentThread() == workThread
				|| Thread.currentThread() == transitionThread) {
			throw new IllegalMonitorStateException(
					"cannot call this from transition or work queue.");
		}
		PrologInterfaceException t = getError();
		while ((getError()) == null) {
			this.wait();
		}

	}

	public synchronized boolean isUp() {
		return state.isUp();
	}

	public synchronized boolean isDown() {
		return state.isDown();
	}

	public synchronized PrologInterfaceException getError() {
		return state.getError();
	}

	private synchronized void setState(State newState) {
		if (newState == state) {
			return;
		}

		String oldStateName = state == null ? "null" : state.getClass()
				.getSimpleName();
		state = newState;
		String newStateName = state == null ? "null" : state.getClass()
				.getSimpleName();
		Debug.debug(oldStateName + " ------> " + newStateName);
		state.enter();
		notifyAll();
	}

	public synchronized void start() {

		transitionThread.enqueue(new NamedWorkRunnable("start") {
			@Override
			public void run() throws PrologInterfaceException {

				setState(state.start());

			}
		});

	}

	public synchronized void stop() {
		transitionThread.enqueue(new NamedWorkRunnable("stop") {
			@Override
			public void run() throws PrologInterfaceException {
				setState(state.stop());
			}
		});
	}

	public synchronized void error(final Throwable e) {
		transitionThread.enqueue(new NamedWorkRunnable("error") {
			@Override
			public void run() throws PrologInterfaceException {
				setState(state.error(e));

			}
		});
	}

	protected synchronized void workDone() {
		transitionThread.enqueue(new NamedWorkRunnable("workDone") {
			@Override
			public void run() throws PrologInterfaceException {
				setState(state.workDone());

			}
		});
	}

	public synchronized void addLifeCycleHook(final LifeCycleHook hook,
			final String id, final String[] dependencies) {
		transitionThread.enqueue(new NamedWorkRunnable("addLifeCycleHook/3") {
			@Override
			public void run() throws PrologInterfaceException {
				setState(state.addLifeCycleHook(hook, id, dependencies));

			}
		});
	}

	public synchronized void removeLifeCycleHook(final String hookId) {
		transitionThread
				.enqueue(new NamedWorkRunnable("removeLifeCycleHook/1") {
					@Override
					public void run() throws PrologInterfaceException {
						setState(state.removeLifeCycleHook(hookId));

					}
				});
	}

	public synchronized void removeLifeCycleHook(final LifeCycleHook hook,
			final String hookId) {

		transitionThread
				.enqueue(new NamedWorkRunnable("removeLifeCycleHook/2") {
					@Override
					public void run() throws PrologInterfaceException {
						setState(state.removeLifeCycleHook(hook, hookId));

					}
				});
	}

	public synchronized void reset() {
		transitionThread.enqueue(new NamedWorkRunnable("reset") {
			@Override
			public void run() throws PrologInterfaceException {
				setState(state.reset());

			}
		});
	}

	public HashMap<String, LifeCycleHookWrapper> getHooks() {
		return hooks;
	}

	public void enqueueWork(WorkRunnable r) {
		workThread.enqueue(r);
	}

	public void clearWorkQueue() {
		workThread.queue.clear();

	}

	public abstract PrologInterface getPrologInterface();

	public abstract PrologSession getShutdownSession()
			throws PrologInterfaceException;

	public abstract PrologSession getInitialSession()
			throws PrologInterfaceException;

	public abstract void startServer() throws Throwable;

	public abstract void stopServer() throws Throwable;

	public abstract boolean isServerRunning() throws Throwable;

	public abstract void disposeSessions() throws Throwable;

}
