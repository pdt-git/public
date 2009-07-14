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
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import java.util.WeakHashMap;

import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.Disposable;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;
import org.cs3.pl.prolog.internal.lifecycle.LifeCycle;

/**
 * convenience implementation of common infrastructure.
 * <p>
 * Subclasses have to implement getSession().
 */
public abstract class AbstractPrologInterface implements PrologInterface {
	protected static final class PifShutdownHook extends Thread {
		WeakHashMap<PrologInterface, Object> pifs;

		private static PifShutdownHook instance;

		private PifShutdownHook() {
			super("PifShutdownHook");
			pifs = new WeakHashMap<PrologInterface, Object>();
			Runtime.getRuntime().addShutdownHook(this);
		}

		static synchronized PifShutdownHook getInstance() {
			if (instance == null) {
				instance = new PifShutdownHook();
			}
			return instance;
		}

		public void run() {
			for (Iterator<PrologInterface> it = pifs.keySet().iterator(); it
					.hasNext();) {
				PrologInterface pif = it.next();
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

	protected class MyLifeCycle extends LifeCycle {

		public MyLifeCycle(String name) {
			super(name);

		}

		
		public PrologSession getInitialSession()
				throws PrologInterfaceException {

			return AbstractPrologInterface.this.getInitialSession();
		}

		
		public PrologInterface getPrologInterface() {

			return AbstractPrologInterface.this;
		}

		
		public PrologSession getShutdownSession()
				throws PrologInterfaceException {
			return AbstractPrologInterface.this.getShutdownSession();
		}

		public void startServer() throws Throwable {
			getStartAndStopStrategy().startServer(AbstractPrologInterface.this);
		}

		public void stopServer() throws Throwable {
			getStartAndStopStrategy().stopServer(AbstractPrologInterface.this);
		}

		public boolean isServerRunning() throws Throwable {
			return getStartAndStopStrategy().isRunning(
					AbstractPrologInterface.this);
		}

		public void disposeSessions() throws Throwable {
			synchronized (sessions) {
				HashSet<WeakReference<PrologSession>> cloned = new HashSet<WeakReference<PrologSession>>(
						sessions);
				for (WeakReference<PrologSession> ref : cloned) {
					Disposable ps = ref.get();
					if (ps != null && !ps.isDisposed()) {
						try{
							ps.dispose();
						} catch(Throwable t){
							Debug.report(t);
						}

					}
				}
				sessions.clear();
			}
		}

	}

	final MyLifeCycle lifecycle;

	private List<String> bootstrapLibraries = new Vector();

	public List<String> getBootstrapLibraries() {
		return bootstrapLibraries;
	}

	public void setBootstrapLibraries(List<String> l) {
		this.bootstrapLibraries = l;
	}

	protected HashSet<WeakReference<PrologSession>> sessions = new HashSet<WeakReference<PrologSession>>();

	private HashMap<String, Vector<PrologInterfaceListener>> listenerLists = new HashMap<String, Vector<PrologInterfaceListener>>();

	public AbstractPrologInterface() {
		PifShutdownHook.getInstance().add(this);
		lifecycle = new MyLifeCycle(this.toString());

	}

	public AbstractPrologInterface(String string) {
		PifShutdownHook.getInstance().add(this);

		lifecycle = new MyLifeCycle(string == null ? this.toString() : string);
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
		lifecycle.addLifeCycleHook(hook, id, dependencies);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologInterface#removeLifeCycleHook(java.lang.String)
	 */
	public void removeLifeCycleHook(String hookId) {
		lifecycle.removeLifeCycleHook(hookId);
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

		try {
			return getSession_internal(LEGACY);//FIXME: a temporary solution. 
		} catch (Throwable t) {
			throw new PrologInterfaceException(t);
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

	public abstract PrologSession getSession_impl(int flags) throws Throwable;

	public PrologSession getSession() throws PrologInterfaceException{
		return getSession(LEGACY);
	}
	
	public PrologSession getSession(int flags) throws PrologInterfaceException {
		
		PLUtil.checkFlags(flags);
		synchronized (lifecycle) {
			if (getError() != null) {
				throw new PrologInterfaceException(getError());
			}
			if (!isUp()) {
				try {
					start();
					waitUntilUp();
				} catch (InterruptedException e) {
					Debug.rethrow(e);
				}
			}
			try {
				return getSession_internal(flags);
			} catch (Throwable t) {
				throw new PrologInterfaceException("Failed to obtain session",
						t);
			}

		}
	}

	

	private PrologSession getSession_internal(int flags) throws Throwable {

		PrologSession s = getSession_impl(flags);
		sessions.add(new WeakReference<PrologSession>(s));
		return s;

	}

	protected void waitUntilUp() throws InterruptedException,
			PrologInterfaceException {
		lifecycle.waitUntilUp();
	}

	/**
	 * overide this if your subclass needs special shutdown sessions.
	 * 
	 * @return
	 * @throws PrologInterfaceException
	 */
	protected PrologSession getShutdownSession()
			throws PrologInterfaceException {
		try {
			return getSession_internal(LEGACY); //FIXME: a temporary solution
		} catch (Throwable t) {
			throw new PrologInterfaceException(t);
		}
	}

	/**
	 * @return Returns the startStrategy.
	 */
	public abstract ServerStartAndStopStrategy getStartAndStopStrategy();

	public boolean isDown() {
		return lifecycle.isDown();
	}

	/**
	 * @return
	 */
	public boolean isUp() {
		return lifecycle.isUp();
	}

	public PrologInterfaceException getError() {
		return lifecycle.getError();
	}

	/**
	 * causes complete re-initialization of the Prolog system, and invalidates
	 * all current sessions.
	 * 
	 * @throws PrologInterfaceException
	 * 
	 * @throws IOException
	 */
	public void restart() throws PrologInterfaceException {
		synchronized (lifecycle) {
			if (getError() != null) {
				reset();
			} else if (isUp()) {
				stop();
			}

			start();
		}
	}

	/**
	 * causes complete re-initialization of the Prolog system, and invalidates
	 * all current sessions.
	 * 
	 * @throws PrologInterfaceException
	 * 
	 * @throws IOException
	 */
	public void reset() throws PrologInterfaceException {
		synchronized (lifecycle) {
			lifecycle.reset();
			try {
				lifecycle.waitUntilDown(true);
			} catch (InterruptedException e) {
				throw error(e);
			}
		}
	}

	/**
	 * override this if you need configurable options. the default
	 * implementation does not have any configuragble options, so it will always
	 * through an IllegalArgumentException..
	 */
	public void setOption(String opt, String value) {
		throw new IllegalArgumentException("option not supported: " + opt);
	}

	public void start() throws PrologInterfaceException {

		synchronized (lifecycle) {
			if (getError() != null) {
				throw new PrologInterfaceException(getError());
			}
			lifecycle.start();
			try {
				lifecycle.waitUntilUp();
			} catch (InterruptedException e) {
				throw new PrologInterfaceException(e);
			}
		}

	}

	public void stop() throws PrologInterfaceException {
		synchronized (lifecycle) {
			if (getError() != null) {
				throw new PrologInterfaceException(getError());
			}
			lifecycle.stop();
			try {
				lifecycle.waitUntilDown(false);
			} catch (InterruptedException e) {
				throw new PrologInterfaceException(e);
			}

		}

	}

	public PrologInterfaceException error(Throwable e) {

		synchronized (lifecycle) {
			if (getError() != null) {
				return getError(); // avoid reentrant calls.
			}
			lifecycle.error(e);
			while (getError() == null) {
				try {
					lifecycle.waitUntilError();
				} catch (InterruptedException e1) {
					;
				}
			}
		}

		return getError();

	}
	public void debug_wakeupPoledSessions(){
		
	}
}
