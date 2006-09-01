/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pl.prolog.internal;

import java.io.IOException;
import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Vector;
import java.util.WeakHashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.Disposable;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;

/**
 * convenience implementation of common infrastructure.
 * <p>
 * Subclasses have to implement getSession().
 */
public abstract class AbstractPrologInterface implements PrologInterface {
	protected static final class PifShutdownHook extends Thread {
		WeakHashMap pifs;

		private static PifShutdownHook instance;

		private PifShutdownHook() {
			super("PifShutdownHook");
			pifs = new WeakHashMap();
			Runtime.getRuntime().addShutdownHook(this);
		}

		static synchronized PifShutdownHook getInstance() {
			if (instance == null) {
				instance = new PifShutdownHook();
			}
			return instance;
		}

		public void run() {
			for (Iterator it = pifs.keySet().iterator(); it.hasNext();) {
				PrologInterface pif = (PrologInterface) it.next();
				if (pif != null) {
					try {
						pif.stop();
					} catch (PrologInterfaceException e) {
						;
					}
				}
			}

		}

		public void add(PrologInterface pif) {
			pifs.put(pif, null);
		}
	}

	private List bootstrapLibraries = new Vector();

	public List getBootstrapLibraries() {
		return bootstrapLibraries;
	}

	public void setBootstrapLibraries(List l) {
		this.bootstrapLibraries = l;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#addPrologInterfaceListener(java.lang.String,
	 *      org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	public void addPrologInterfaceListener(String subject,
			PrologInterfaceListener l) {
		synchronized (listenerLists) {
			Vector list = (Vector) listenerLists.get(subject);
			if (list == null) {
				list = new Vector();
				listenerLists.put(subject, list);
			}
			if (!list.contains(l)) {
				list.add(l);
			}
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#removePrologInterfaceListener(java.lang.String,
	 *      org.cs3.pl.prolog.PrologInterfaceListener)
	 */
	public void removePrologInterfaceListener(String subject,
			PrologInterfaceListener l) {
		synchronized (listenerLists) {
			Vector list = (Vector) listenerLists.get(subject);
			if (list == null) {
				return;
			}
			if (list.contains(l)) {
				list.remove(l);
			}

		}

	}

	public static final int DOWN = 0;

	public static final int ERROR = -1;

	public static final int SHUT_DOWN = 3;

	public static final int START_UP = 1;

	public static final int UP = 2;

	protected HookHelper hookHelper = new HookHelper(this);

	protected Collection sessions = new LinkedList();

	private ServerStartAndStopStrategy startAndStopStrategy;

	private int state = DOWN;

	protected Object stateLock = new Object();

	private HashMap listenerLists = new HashMap();

	/*
	 * Debug facility it was a common mistake in the past to call getSession()
	 * during startup from the same thread that run the startup (i.e. during
	 * init hooks) This field saves the thread triggereing the startup/shutdown.
	 * In getsessoin, it checks wether it is the same thread, and throws a
	 * helpfull exception to hint the blind, rather than just pissing everyone
	 * of by locking the thread. :-)
	 * 
	 * This can propably be removed someday soon.
	 */
	Thread theThreadWhoDidIt = null;

	public AbstractPrologInterface() {
		PifShutdownHook.getInstance().add(this);
	}

	protected void finalize() throws Throwable {
		stop();
		super.finalize();
	}

	/**
	 * @param hook
	 * @param id
	 * @param dependsOn
	 */
	public void addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies) {
		hookHelper.addLifeCycleHook(hook, id, dependencies);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterface#removeLifeCycleHook(java.lang.String)
	 */
	public void removeLifeCycleHook(String hookId) {
		hookHelper.removeLifeCycleHook(hookId);
	}

	/**
	 * 
	 * override this if your subclass needs special initial Sessions
	 * 
	 * @param initSession
	 *            a session obtained from getInitialSession()
	 */
	protected void disposeInitialSession(PrologSession initSession) {
		initSession.dispose();
	}

	/**
	 * overide this if your subclass needs special shutdown sessions.
	 * 
	 * @param s
	 *            a session obtained from getShutdownSession()
	 */
	protected void disposeShutdownSession(PrologSession s) {
		s.dispose();

	}

	/**
	 * override this if your subclass needs special initial Sessions
	 * 
	 * @return
	 * @throws PrologInterfaceException 
	 */
	protected PrologSession getInitialSession() throws PrologInterfaceException {
		synchronized (stateLock) {
			if (getState() != START_UP) {
				throw new IllegalStateException(
						"cannot create initial session, not in START_UP state.");
			}
			try {
				return getSession_internal();
			} catch (Throwable t) {
				Debug.rethrow(t);
				return null;
			}
		}
	}

	/**
	 * override this if you need configurable options. the default
	 * implementation does not have any configuragble options, so it will always
	 * through an IllegalArgumentException..
	 */
	public String getOption(String opt) {
		throw new IllegalArgumentException("option not supported: " + opt);
	}

	public abstract PrologSession getSession_impl() throws Throwable;

	public PrologSession getSession() throws PrologInterfaceException {
		synchronized (stateLock) {
			if(DOWN==getState()){
				start();
			}
			if (START_UP == getState()) {
				if (theThreadWhoDidIt == Thread.currentThread()) {
					Debug
							.error("getSession() called from init thread. Please read the api docs for LifeCycleHook.onInit(PrologSession).");
					throw new IllegalThreadStateException(
							"You cannot call getSession() from the init thread during pif startup.");
				}
				waitUntilUp();
			}
			if (UP != getState()) {
				throw new IllegalStateException(
						"Cannot create session. Not in UP state.");
			}
			try {
				return getSession_internal();
			} catch (Throwable t) {
				throw new PrologInterfaceException("Failed to obtain session",t);
			}
		}
	}

	private PrologSession getSession_internal() throws Throwable {
		synchronized (stateLock) {
			PrologSession s = getSession_impl();
			sessions.add(new WeakReference(s));
			return s;

		}
	}

	protected void waitUntilUp() throws IllegalStateException {
		if (getState() != START_UP && getState() != UP) {
			throw new IllegalStateException("Not in state START_UP or UP");
		}
		synchronized (stateLock) {
			while (!isUp()) {
				try {
					stateLock.wait();
				} catch (InterruptedException e) {
					throw new IllegalStateException("interupted");
				}
				if (ERROR == getState()) {
					throw new IllegalStateException(
							"Error while waiting for pif to come up.");
				}
				if (DOWN == getState()) {
					throw new IllegalStateException(
							"PrologInterface did not come up.");
				}
			}
		}
	}

	/**
	 * overide this if your subclass needs special shutdown sessions.
	 * 
	 * @return
	 * @throws PrologInterfaceException 
	 */
	protected PrologSession getShutdownSession() throws PrologInterfaceException {
		synchronized (stateLock) {
			if (getState() != SHUT_DOWN) {
				throw new IllegalStateException(
						"cannot create shutdown session, not in SHUT_DOWN state.");
			}
			try {
				return getSession_internal();
			} catch (Throwable t) {
				Debug.rethrow(t);
				return null;
			}
		}
	}

	/**
	 * @return Returns the startStrategy.
	 */
	public ServerStartAndStopStrategy getStartAndStopStrategy() {
		return startAndStopStrategy;
	}

	/**
	 * @return Returns the state.
	 */
	public int getState() {
		synchronized (stateLock) {
			return state;
		}
	}

	public boolean isDown() {
		return (getState() == DOWN);
	}

	/**
	 * @return
	 */
	public boolean isUp() {
		return getState() == UP;
	}

	/**
	 * causes complete re-initialization of the Prolog system, and invalidates
	 * all current sessions.
	 * @throws PrologInterfaceException 
	 * 
	 * @throws IOException
	 */
	public void restart() throws PrologInterfaceException  {
		stop();
		start();
	}

	/**
	 * override this if you need configurable options. the default
	 * implementation does not have any configuragble options, so it will always
	 * through an IllegalArgumentException..
	 */
	public void setOption(String opt, String value) {
		throw new IllegalArgumentException("option not supported: " + opt);
	}

	protected static String stateString(int state) {
		switch (state) {
		case DOWN:
			return "DOWN";
		case START_UP:
			return "START_UP";
		case UP:
			return "UP";
		case SHUT_DOWN:
			return "SHUT_DOWN";
		case ERROR:
			return "ERROR";

		default:
			return "undefined";
		}
	}

	/*
	 * there is no real state machiene or something: This is just an attemp to
	 * synchronize (and detect errors in) the state flow of the pif. Basicaly at
	 * some point you say: "Now get to state XY" and this method will check if
	 * this transition is legal (i.e. expected to happen) Otherwise this method
	 * throws an exception. It is called by state-changing methods start()
	 * stop() and emergencyStop()
	 * 
	 */
	protected void setState(int newState) throws IllegalStateException {
		synchronized (stateLock) {
			if (state == newState) {
				throw new IllegalStateException(
						"transition not allowed. (no change!)");
			}
			// check if the transition is allowed.
			String transitionMessage = stateString(this.state) + " --> "
					+ stateString(newState);
			switch (newState) {
			case ERROR:
				Debug.info("PLIF ERROR");
				break;
			case DOWN:
				if (this.state != SHUT_DOWN && this.state != ERROR) {
					throw new IllegalStateException("transition not allowed: "
							+ transitionMessage);
				}
				Debug.info("PLIF DOWN");
				break;
			case START_UP:
				if (this.state != DOWN) {
					throw new IllegalStateException("transition not allowed: "
							+ transitionMessage);
				}
				Debug.info("PLIF START_UP");
				break;
			case UP:
				if (this.state != START_UP) {
					throw new IllegalStateException("transition not allowed: "
							+ transitionMessage);
				}
				Debug.info("PLIF UP");
				break;
			case SHUT_DOWN:
				if (this.state == UP) {
					Debug.info("PLIF SHUT_DOWN");
				} else {
					throw new IllegalStateException("transition not allowed: "
							+ transitionMessage);
				}
				break;
			default:
				throw new IllegalArgumentException("Illegal state:" + newState);
			}
			this.state = newState;
			theThreadWhoDidIt = Thread.currentThread();
			stateLock.notifyAll();
		}
	}

	public void start() throws PrologInterfaceException {
		try {
			setState(START_UP);

		} catch (IllegalStateException e) {
			Debug.warning("I will not start: not in DOWN state!");
			return;
		}

		if (startAndStopStrategy.isRunning(this)) {
			Debug.warning("ahem... the port is in use. \n"
					+ "Trying to connect & shutdown, but this may not work.");
			startAndStopStrategy.stopServer(this);
		}
		startAndStopStrategy.startServer(this);
		PrologSession initSession = getInitialSession();
		hookHelper.onInit(initSession);
		disposeInitialSession(initSession);
		setState(UP);
		hookHelper.afterInit();

	}

	public synchronized void stop() throws PrologInterfaceException {
		try {
			setState(SHUT_DOWN);
		} catch (IllegalStateException e) {
			Debug.warning("I will not shut down: not in UP state.");
			return;
		}
		try {
			PrologSession s = getShutdownSession();
			if (s != null) {
				hookHelper.beforeShutdown(s);
				try {
					disposeShutdownSession(s);
				} catch (Throwable t) {
					Debug.error("could not dispose shutdown session.");
					Debug.report(t);
				}
			}
			synchronized (sessions) {
				for (Iterator i = sessions.iterator(); i.hasNext();) {
					WeakReference element = (WeakReference) i.next();
					Disposable ps = (Disposable) element.get();
					if (ps != null && !ps.isDisposed())
						try {
							ps.dispose();
						} catch (Throwable t) {
							handleException(t);
						}
					i.remove();
				}
			}
			startAndStopStrategy.stopServer(this);
			setState(DOWN);
		} catch (Throwable t) {

			Debug
					.error("Could not shut down because of uncaught exception(s).");
			handleException(t);
		}
	}

	public synchronized void emergencyStop() {

		try {
			synchronized (sessions) {
				for (Iterator i = sessions.iterator(); i.hasNext();) {
					WeakReference element = (WeakReference) i.next();
					Disposable ps = (Disposable) element.get();
					if (ps != null && !ps.isDisposed())
						try {
							ps.dispose();
						} catch (Throwable t) {
							Debug.report(t);
							Debug
									.error("problem is ignored, pif already in state ERROR");
						}
					i.remove();
				}
			}
			hookHelper.onError(this);
			startAndStopStrategy.stopServer(this);
			setState(DOWN);
		} catch (Throwable t) {
			Debug.error("Emergency stop failed! I don't know what to do now.");
			Debug.rethrow(t);
		}
	}

	/**
	 * @param startAndStopStrategy
	 *            The startAndStopStrategy to set.
	 */
	public void setStartAndStopStrategy(
			ServerStartAndStopStrategy startAndStopStrategy) {
		this.startAndStopStrategy = startAndStopStrategy;
	}

	/**
	 * @param subject2
	 * @param string
	 */
	public void fireUpdate(String subject, String event) {
		Vector listeners = (Vector) listenerLists.get(subject);
		if (listeners == null) {
			return;
		}
		PrologInterfaceEvent e = new PrologInterfaceEvent(this, subject, event);

		Vector cloned = null;
		synchronized (listeners) {
			cloned = (Vector) listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PrologInterfaceListener l = (PrologInterfaceListener) it.next();
			l.update(e);
		}
	}

	public void handleException(Throwable e) throws PrologInterfaceException {
		if (getState() == ERROR) {
			Debug.report(e);
			Debug.error("ignoring problem, pif is already in state ERROR");
			return;
		}
		if (getState() == DOWN) {
			Debug.report(e);
			Debug.error("ignoring problem, pif is down");
			return;
		}
		setState(ERROR);
		Debug.error("pif encountered fatal error.");
		Debug.report(e);
		Debug.error("performing emergency shutdown.");
		emergencyStop();
		Debug.error("rethrowing exception");
		throw new PrologInterfaceException(e);
	}
}
