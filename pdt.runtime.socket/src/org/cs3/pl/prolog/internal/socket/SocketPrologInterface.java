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

package org.cs3.pl.prolog.internal.socket;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.PreferenceProvider;
import org.cs3.pl.prolog.AsyncPrologSession;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.ServerStartAndStopStrategy;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.prolog.internal.ReusablePool;

public class SocketPrologInterface extends AbstractPrologInterface implements SocketPrologInterfacePreferences {

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

	private boolean createLogs;
	private String serverLogDir;
		

	public void setPort(int port) {
		this.port = port;
	}
	public void setServerPort(String port) {
		this.port = Integer.parseInt(port);
	}
	public void setUseSessionPooling(String useSessionPooling) {
		setUseSessionPooling(Boolean.parseBoolean(useSessionPooling));
	}
	public void setUseSessionPooling(boolean useSessionPooling) {
		this.useSessionPooling = useSessionPooling;
		pool = useSessionPooling ? new ReusablePool() : null;
	}
	public void setCreateLogs(boolean createLogs) {
		this.createLogs = createLogs;
	}
	public void setCreateLogs(String createLogs) {
		this.createLogs = Boolean.parseBoolean(createLogs);
	}
	public boolean isCreateLogs() {
		return createLogs;
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
		setServerPort(provider.getPreference(SocketPrologInterfacePreferences.PREF_PORT));
		setHidePlwin(provider.getPreference(SocketPrologInterfacePreferences.PREF_HIDE_PLWIN));
		setCreateLogs(provider.getPreference(SocketPrologInterfacePreferences.PREF_CREATE_SERVER_LOGS));
		setUseSessionPooling(provider.getPreference(SocketPrologInterfacePreferences.PREF_USE_POOL));
		setServerLogDir(provider.getPreference(SocketPrologInterfacePreferences.PREF_SERVER_LOGDIR));		

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
		setCreateLogs(false);
		setUseSessionPooling("true");
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
	public void debug_wakeupPoledSessions() {
		int S =pool.getMaxTotalSize();
		PrologSession[] sessions = new PrologSession[S];
		for(int i=0;i<sessions.length;i++){
			try {
				sessions[i]=getSession(PrologInterface.NONE);
			} catch (PrologInterfaceException e) {
				;
			}
		}
		for (PrologSession session : sessions) {
			if(session!=null){
				session.dispose();
			}
		}
	}
	
	 // =============================================================
	 // modified from factory
	 // =============================================================
    
	public static PrologInterface newInstance() {

		return newInstance("egal", null);
	}

	public static PrologInterface newInstance(String fqn, String name) {
		// fqn ist egal, da diese Methode die allgemeinere aus der abstrakten
		// oberklasse überschreibt
		return new SocketPrologInterface(name);
	}
	@Override
	public boolean hasError() {
		return getError()!=null;
	}
	
}