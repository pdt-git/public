/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.prolog.connector.internal.process;

import java.lang.ref.WeakReference;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.WeakHashMap;

import org.cs3.prolog.connector.Connector;
import org.cs3.prolog.connector.common.PreferenceProvider;
import org.cs3.prolog.connector.common.logging.Debug;
import org.cs3.prolog.connector.cterm.CTermUtil;
import org.cs3.prolog.connector.internal.lifecycle.LifeCycle;
import org.cs3.prolog.connector.lifecycle.LifeCycleHook;
import org.cs3.prolog.connector.process.PrologInterface;
import org.cs3.prolog.connector.process.PrologInterfaceException;
import org.cs3.prolog.connector.process.StartupStrategy;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.Disposable;
import org.cs3.prolog.connector.session.PrologSession;

/**
 * convenience implementation of common infrastructure.
 * <p>
 * Subclasses have to implement getSession().
 */
public abstract class AbstractPrologInterface implements PrologInterface {

	protected HashSet<WeakReference<? extends Disposable>> sessions = new HashSet<WeakReference<? extends Disposable>>();
	private StartupStrategy startupStrategy;
	private final MyLifeCycle lifecycle;
	private int defaultSessionFlag = LEGACY;

	// Options [Start]

	private boolean standAloneServer = false;
	private String host;
	private String osInvocation;
	private String executablePath;
	private String commandLineArguments;
	private String additionalStartupFile;
	private String environment;
	private int timeout;
	private String fileSearchPath;

	private HashMap<String, Object> attributes = new HashMap<String, Object>();

	public AbstractPrologInterface() {
		this(null);
	}
	
	public AbstractPrologInterface(String string) {
		PifShutdownHook.getInstance().add(this);
		lifecycle = new MyLifeCycle(string == null ? this.toString() : string);
	}


	// Options [Start]
	public void setStandAloneServer(String standAloneServer) {
		setStandAloneServer(Boolean.parseBoolean(standAloneServer));
	}

	@Override
	public void setStandAloneServer(boolean standAloneServer) {
		if (isDown()) {
			this.standAloneServer = standAloneServer;
		} else {
			throw new IllegalStateException("Cannot change standalone flag while in use.");
		}

	}

	@Override
	public boolean isStandAloneServer() {
		return standAloneServer;
	}

	@Override
	public void setFileSearchPath(String fileSearchPath) {
		this.fileSearchPath = fileSearchPath;
	}

	@Override
	public String getFileSearchPath() {
		return fileSearchPath;
	}

	@Override
	public void setHost(String value) {
		this.host = value;
	}

	@Override
	public String getHost() {
		return host;
	}

	@Override
	public void setTimeout(String timeout) {
		if(timeout.equals("") || timeout==null) {
			Debug.warning("Invalid Prolog Interface timeout value: " + timeout + ", using 15 sec instead");
			timeout="15000";
		}
		this.timeout = Integer.parseInt(timeout);
	}

	@Override
	public int getTimeout() {
		return timeout;
	}

	public String getOSInvocation() {
		return osInvocation;
	}
	
	public void setOSInvocation(String osInvocation) {
		this.osInvocation = osInvocation;
	}
	
	public String getExecutablePath() {
		return executablePath;
	}
	
	public void setExecutablePath(String executablePath) {
		this.executablePath = executablePath;
	}
	
	public String getCommandLineArguments() {
		return commandLineArguments;
	}
	
	public void setCommandLineArguments(String commandLineArguments) {
		this.commandLineArguments = commandLineArguments;
	}
	
	public String getAdditionalStartupFile() {
		return additionalStartupFile;
	}
	
	public void setAdditionalStartupFile(String additionalStartupFile) {
		this.additionalStartupFile = additionalStartupFile;
	}
	
	@Override
	public void setEnvironment(String environment) {
		this.environment = environment;
	}

	@Override
	public String getEnvironment() {
		return environment;
	}

	@Override
	public Object getAttribute(String attribute) {
		return attributes.get(attribute);
	}

	@Override
	public void setAttribute(String attribute, Object value) {
		attributes.put(attribute, value);
	}

	@Override
	public void initOptions(PreferenceProvider provider) {
		setHost(provider.getPreference(Connector.PREF_HOST));
		setOSInvocation(provider.getPreference(Connector.PREF_INVOCATION));
		setExecutablePath(provider.getPreference(Connector.PREF_EXECUTABLE));
		setCommandLineArguments(provider.getPreference(Connector.PREF_COMMAND_LINE_ARGUMENTS));
		setAdditionalStartupFile(provider.getPreference(Connector.PREF_ADDITIONAL_STARTUP));
		setEnvironment(provider.getPreference(Connector.PREF_ENVIRONMENT));
		setTimeout(provider.getPreference(Connector.PREF_TIMEOUT));
	}

	/************************************************/
	/**** Options [End] *****/
	/************************************************/

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

		@Override
		public void run() {
			for (Iterator<PrologInterface> it = pifs.keySet().iterator(); it.hasNext();) {
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

		@Override
		public PrologSession getInitialSession() throws PrologInterfaceException {
			return AbstractPrologInterface.this.getInitialSession();
		}

		@Override
		public PrologInterface getPrologInterface() {
			return AbstractPrologInterface.this;
		}

		@Override
		public PrologSession getShutdownSession() throws PrologInterfaceException {
			return AbstractPrologInterface.this.getShutdownSession();
		}

		@Override
		public void startServer() throws Throwable {
			getStartAndStopStrategy().startServer(AbstractPrologInterface.this);
		}

		@Override
		public void stopServer() throws Throwable {
			getStartAndStopStrategy().stopServer(AbstractPrologInterface.this);
		}

		@Override
		public boolean isServerRunning() throws Throwable {
			return getStartAndStopStrategy().isRunning(AbstractPrologInterface.this);
		}

		@Override
		public void disposeSessions() throws Throwable {
			synchronized (sessions) {
				HashSet<WeakReference<? extends Disposable>> cloned = new HashSet<WeakReference<? extends Disposable>>(sessions);
				for (WeakReference<? extends Disposable> ref : cloned) {
					Disposable ps = ref.get();
					if (ps != null && !ps.isDisposed()) {
						try {
							ps.dispose();
						} catch (Throwable t) {
							Debug.report(t);
						}

					}
				}
				sessions.clear();
			}
		}

	}

	@Override
	protected void finalize() throws Throwable {
		stop();
		super.finalize();
	}

	/**
	 * @param hook
	 * @param id
	 * @param dependencies
	 */
	@Override
	public void addLifeCycleHook(LifeCycleHook hook, String id, String[] dependencies) {
		lifecycle.addLifeCycleHook(hook, id, dependencies);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * org.cs3.pl.prolog.PrologInterface#removeLifeCycleHook(java.lang.String)
	 */
	@Override
	public void removeLifeCycleHook(String hookId) {
		lifecycle.removeLifeCycleHook(hookId);
	}

	@Override
	public void removeLifeCycleHook(final LifeCycleHook hook, final String hookId) {
		lifecycle.removeLifeCycleHook(hook, hookId);
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
			return getSession_internal(defaultSessionFlag);// FIXME: a temporary solution.
		} catch (Throwable t) {
			throw new PrologInterfaceException(t);
		}

	}

	/**
	 * override this if you need configurable options. the default
	 * implementation does not have any configurable options, so it will always
	 * through an IllegalArgumentException..
	 */
	public String getOption(String opt) {
		throw new IllegalArgumentException("option not supported: " + opt);
	}

	public abstract PrologSession getSession_impl(int flags) throws Throwable;

	@Override
	public PrologSession getSession() throws PrologInterfaceException {
		return getSession(defaultSessionFlag);
	}

	@Override
	public PrologSession getSession(int flags) throws PrologInterfaceException {

		CTermUtil.checkFlags(flags);
		synchronized (lifecycle) {
			if (getError() != null) {
				restart();
				if (getError() != null) {
					throw new PrologInterfaceException(getError());
				}
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
				throw new PrologInterfaceException("Failed to obtain session", t);
			}

		}
	}

	private PrologSession getSession_internal(int flags) throws Throwable {

		PrologSession s = getSession_impl(flags);
		sessions.add(new WeakReference<PrologSession>(s));
		return s;

	}

	protected void waitUntilUp() throws InterruptedException, PrologInterfaceException {
		lifecycle.waitUntilUp();
	}

	/**
	 * overide this if your subclass needs special shutdown sessions.
	 * 
	 * @return
	 * @throws PrologInterfaceException
	 */
	protected PrologSession getShutdownSession() throws PrologInterfaceException {
		try {
			return getSession_internal(defaultSessionFlag); // FIXME: a temporary solution
		} catch (Throwable t) {
			throw new PrologInterfaceException(t);
		}
	}

	/**
	 * @return Returns the startStrategy.
	 */
	public abstract ServerStartAndStopStrategy getStartAndStopStrategy();

	@Override
	public boolean isDown() {
		return lifecycle.isDown();
	}

	/**
	 * @return
	 */
	@Override
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
	 */
	@Override
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
	 */
	@Override
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

	@Override
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

			//			reconsultFiles();
		}

	}

	@Override
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

	public abstract AsyncPrologSession getAsyncSession_impl(int flags) throws Throwable;

	@Override
	public AsyncPrologSession getAsyncSession() throws PrologInterfaceException {
		return getAsyncSession(defaultSessionFlag);
	}

	@Override
	public AsyncPrologSession getAsyncSession(int flags) throws PrologInterfaceException {
		CTermUtil.checkFlags(flags);
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
				return getAsyncSession_internal(flags);
			} catch (Throwable t) {
				throw new PrologInterfaceException("Failed to obtain session", t);
			}
		}
	}

	private AsyncPrologSession getAsyncSession_internal(int flags) throws Throwable {
		AsyncPrologSession asyncSession = getAsyncSession_impl(flags);
		sessions.add(new WeakReference<AsyncPrologSession>(asyncSession));
		return asyncSession;
	}

	@Override
	public StartupStrategy getStartupStrategy() {
		return startupStrategy;
	}
	
	@Override
	public void setStartupStrategy(StartupStrategy startupStrategy) {
		this.startupStrategy = startupStrategy;
	}

	@Override
	public void setDefaultSessionFlag(int flag) {
		defaultSessionFlag = flag;
	}
	
	@Override
	public int getDefaultSessionFlag() {
		return defaultSessionFlag;
	}
	
	@Override
	public List<Map<String, Object>> queryAll(String... predicates) throws PrologInterfaceException {
		return queryAll(getDefaultSessionFlag(), predicates);
	}
	
	/**
	 * Wrapper for {@link PrologSession#queryAll(String)}
	 * Executes queryAll for every predicate given in predicates.
	 * 
	 * @param predicates
	 * @return result List contains a result map for each predicate queried 
	 * @throws PrologInterfaceException
	 */
	@Override
	public List<Map<String, Object>> queryAll(int flag, String... predicates) throws PrologInterfaceException {
		
		StringBuffer buf = new StringBuffer();
		boolean first = true;
		for (String s : predicates) {
			if (first) {
				first = false;
			} else {
				buf.append(",");
			}
			buf.append(s);
		}
		List<Map<String, Object>> result = null;		 
		PrologSession session = null;
		try {
		    session = getSession(flag);
			result = session.queryAll(buf.toString());
		} finally {
		    if (session != null)
		      session.dispose();
		}
		return result;
	}
	

	@Override
	public Map<String, Object> queryOnce(String... predicates) throws PrologInterfaceException {
		return queryOnce(getDefaultSessionFlag(), predicates);
	}
	
	/**
	 * Wrapper for {@link PrologSession#queryOnce(String)}
	 * Executes queryOnce for every predicate given in predicates.
	 * 
	 * @param predicates
	 * @return result Map of the last predicate queried 
	 * @throws PrologInterfaceException
	 */
	@Override
	public Map<String, Object> queryOnce(int flag, String... predicates) throws PrologInterfaceException {
		
		StringBuffer buf = new StringBuffer();
		boolean first = true;
		for (String s : predicates) {
			if (first) {
				first = false;
			} else {
				buf.append(",");
			}
			buf.append(s);
		}
		
		Map<String, Object> result = null;		 
		PrologSession session = null;
		try {
			session = getSession(flag);
			result = session.queryOnce(buf.toString());
		} finally {
		    if (session != null)
		      session.dispose();
		}
		return result;
	}


	

}


