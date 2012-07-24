/* $LICENSE_MSG$(ld) */

package org.cs3.prolog.internal.pif.socket;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.common.PreferenceProvider;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologRuntime;
import org.cs3.prolog.internal.pif.AbstractPrologInterface;
import org.cs3.prolog.internal.pif.ServerStartAndStopStrategy;
import org.cs3.prolog.internal.session.socket.AsyncSocketSession;
import org.cs3.prolog.internal.session.socket.SocketSession;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.AsyncPrologSession;
import org.cs3.prolog.session.PrologSession;

public class SocketPrologInterface extends AbstractPrologInterface {

	private class InitSession extends SocketSession {
		public InitSession(SocketClient client, AbstractPrologInterface pif,int flags)
				throws IOException {
			super(client, pif,flags);
		}

		@Override
		public void dispose() {
			Debug.warning("Ignoring attempt to dispose an initial session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private class ShutdownSession extends SocketSession {
		public ShutdownSession(SocketClient client, AbstractPrologInterface pif, int flags)
				throws IOException {
			super(client, pif,flags);
		}

		@Override
		public void dispose() {
			Debug.warning("Ignoring attempt to dispose a shutdown session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private boolean useSessionPooling = true;
	private int port = 9999;
	private boolean hidePlwin;

	private String serverLogDir;
		

	public void setPort(int port) {
		this.port = port;
	}
	public void setServerPort(String port) {
		this.port = Integer.parseInt(port);
	}
	public void setUseSessionPooling(boolean useSessionPooling) {
		this.useSessionPooling = useSessionPooling;
		pool = useSessionPooling ? new ReusablePool() : null;
	}
	public int getPort() {
		return port;
	}	
	public boolean isHidePlwin() {
		return hidePlwin;
	}
	public void setHidePlwin(boolean hidePlwin) {
		this.hidePlwin = hidePlwin;
	}
	public void setHidePlwin(String hidePlwin) {
		this.hidePlwin = Boolean.parseBoolean(hidePlwin);
	}
	public void setServerLogDir(String path){
		serverLogDir = path;
	}
	public String getServerLogDir(){
		return serverLogDir;
	}
	@Override
	public void initOptions(PreferenceProvider provider) {
		super.initOptions(provider);
		setServerPort(provider.getPreference(PrologRuntime.PREF_PORT));
		setHidePlwin(provider.getPreference(PrologRuntime.PREF_HIDE_PLWIN));
		setUseSessionPooling(true);
		setServerLogDir(provider.getPreference(PrologRuntime.PREF_SERVER_LOGDIR));		

	}
	
	
	/************************************************/
	/**** Options [End] *****/
	/************************************************/	


	private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;
	
	private File lockFile;
	private ServerStartAndStopStrategy startAndStopStrategy;


	public SocketPrologInterface(String name) {		
		super(name);		
		setDefaults();
		setStartAndStopStrategy(new SocketServerStartAndStopStrategy());
	}
	
	public void setDefaults() {
		setHidePlwin(true);
		setUseSessionPooling(true);
		setServerLogDir(System.getProperty("java.io.tmpdir"));		
	}
	
	@Override
	public PrologSession getSession_impl(int flags) throws Throwable {
		ReusableSocket socket = null;
		try {
			if (useSessionPooling) {
				socket = (ReusableSocket) pool
						.findInstance(ReusableSocket.class);
			}
			if (socket == null) {
				socket = new ReusableSocket(getHost(), port);
				Debug.info("sync session creating new ReusableSocket: " + socket.getLocalPort());

			} else {
				Debug.info("sync session reusing old ReusableSocket: " + socket.getLocalPort());
			}

			SocketClient client = new SocketClient(socket);
			client.setPool(pool);
			SocketSession s = new SocketSession(client, this,flags);

			return s;
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	@Override
	public AsyncPrologSession getAsyncSession_impl(int flags) throws Throwable {
		ReusableSocket socket = null;
		try {
			if (useSessionPooling) {
				socket = (ReusableSocket) pool
						.findInstance(ReusableSocket.class);
			}
			if (socket == null) {
				socket = new ReusableSocket(getHost(), port);
				Debug.info("async session creating new ReusableSocket: " + socket.getLocalPort());
			} else {
				Debug.info("async session reusing old ReusableSocket: " + socket.getLocalPort());
			}
			SocketClient client = new SocketClient(socket);
			client.setParanoiaEnabled(false);
			client.setPool(pool);
			
			AsyncPrologSession s = new AsyncSocketSession(client, this,flags);

			return s;
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeInitialSession(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	protected void disposeInitialSession(PrologSession initSession) {
		InitSession s = (InitSession) initSession;
		s.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeShutdownSession(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	protected void disposeShutdownSession(PrologSession s) {
		ShutdownSession ss = (ShutdownSession) s;
		ss.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getInitialSession()
	 */
	@Override
	protected PrologSession getInitialSession() throws PrologInterfaceException {
		try {
			//FIXME: LEGACY for now, should be specified by client somehow.
			return new InitSession(new SocketClient(getHost(), port), this,LEGACY);
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getShutdownSession()
	 */
	@Override
	protected PrologSession getShutdownSession()
			throws PrologInterfaceException {
		try {
			//FIXME: LEGACY for now, should be specified by client somehow.
			return new ShutdownSession(new SocketClient(getHost(), port), this,LEGACY);
		} catch (Throwable e) {
			throw error(e);
			
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
	 */
	@Override
	public  void stop() throws PrologInterfaceException {
		try {
			super.stop();
		} finally {
			if (pool != null) {
				pool.clear();
			}
		}
	}

	@Override
	public  PrologInterfaceException error(Throwable e) {
		try {
			super.error(e);
		} finally {
			if (pool != null) {
				pool.clear();
			}
		}
		return getError();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
	 */
	@Override
	public void start() throws PrologInterfaceException {
		Debug.info("pdt: Start Socket ");
		if (pool != null) {
			pool.clear();
		}
		super.start();
	}

	public void setLockFile(File string) {
		this.lockFile = string;

	}

	public File getLockFile() {
		return lockFile;
	}

	private File errorLogFile;
	
	public void setErrorLogFile(File file) {
		this.errorLogFile = file;

	}

	public File getErrorLogFile() {
		return errorLogFile;
	}

	
	
	@Override
	public ServerStartAndStopStrategy getStartAndStopStrategy() {	
		return this.startAndStopStrategy;
	}

	/**
	 * @param startAndStopStrategy
	 *            The startAndStopStrategy to set.
	 */
	public void setStartAndStopStrategy(ServerStartAndStopStrategy startAndStopStrategy) {
		this.startAndStopStrategy = startAndStopStrategy;
	}

	@Override
	public boolean hasError() {
		return getError()!=null;
	}

	/**
	 * Wrapper for {@link PrologSession#queryOnce(String)}
	 * Executes queryOnce for every predicate given in predicates.
	 * 
	 * @param pif
	 * @param predicates
	 * @return result Map of the last predicate queried 
	 * @throws PrologInterfaceException
	 */
	@Override
	public Map<String, Object> queryOnce(String... predicates) throws PrologInterfaceException {
		
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
			session = getSession(PrologInterface.LEGACY);
			result = session.queryOnce(buf.toString());
		} finally {
		    if (session != null)
		      session.dispose();
		}
		return result;
	}

	/**
	 * Wrapper for {@link PrologSession#queryAll(String)}
	 * Executes queryAll for every predicate given in predicates.
	 * 
	 * @param pif
	 * @param predicates
	 * @return result List contains a result map for each predicate queried 
	 * @throws PrologInterfaceException
	 */
	@Override
	public List<Map<String, Object>> queryAll(String... predicates) throws PrologInterfaceException {
		
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
		    session = getSession(PrologInterface.LEGACY);
			result = session.queryAll(buf.toString());
		} finally {
		    if (session != null)
		      session.dispose();
		}
		return result;
	}
	
}

