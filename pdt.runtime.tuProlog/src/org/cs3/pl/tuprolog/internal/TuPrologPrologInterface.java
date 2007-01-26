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

package org.cs3.pl.tuprolog.internal;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.common.Debug;
import org.cs3.pl.common.ResourceFileLocator;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceFactory;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.pl.prolog.internal.AbstractPrologInterface;
import org.cs3.pl.tuprolog.internal.test.ReusableSocket;

import alice.tuprolog.InvalidLibraryException;
import alice.tuprolog.InvalidTheoryException;
import alice.tuprolog.Prolog;
import alice.tuprolog.Theory;
import alice.tuprolog.event.WarningListener;

public class TuPrologPrologInterface extends AbstractPrologInterface {

	private class InitSession extends TuPrologSession {
		public InitSession(TuPrologPrologInterface pif)
				throws IOException {
			super(pif);
		}

		public void dispose() {
			Debug.warning("Ignoring attempt to dispose an initial session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private class ShutdownSession extends TuPrologSession {
		public ShutdownSession( TuPrologPrologInterface pif)
				throws IOException {
			super(pif);
		}

		public void dispose() {
			Debug.warning("Ignoring attempt to dispose a shutdown session!");
			Debug.warning("called from here:");
			Thread.dumpStack();
		}

		public void doDispose() {
			super.dispose();
		}
	}

	private int port = -1;

	private boolean standAloneServer = false;

	private boolean useSessionPooling = true;

//	private ReusablePool pool = useSessionPooling ? new ReusablePool() : null;

	public final static String BOOT_DIR = "pif.engine_dir";

	public final static String STANDALONE = "pif.standalone";

	public static final String ENGINE_FILE = "pif.engine_file";

	public static final String MAIN_FILE = "pif.main_file";

	public final static String USE_POOL = "pif.use_pool";

	public final static String TIMEOUT = "pif.timeout";

	public static final String WARNING = "warning";
	
	/**
	 * @author abdelhal
	 * 
	 * Sessions management for Sync Library.
	 */
	public static int currentActiveSession = 0;


	private PrologInterfaceFactory factory;

	private ResourceFileLocator locator;

	private File lockFile;

	private int timeout;

	private boolean hidePlwin;

	private TuProlog engine;

	public TuPrologPrologInterface(PrologInterfaceFactory factory) throws InvalidTheoryException, IOException, InvalidLibraryException {
		this.factory = factory;
		Debug.setDebugLevel(Debug.LEVEL_ERROR);
    	engine = new TuProlog();
/*
 * FIXME Already loaded within TuProlog
    	Theory theory = new Theory(
				":- op(1150, fx, dynamic). \n" +
				":- op(1150, fx, multifile).\n"+
				
				"dynamic(A) :- is_dynamic(A),!.\n" +
				"dynamic(A) :- assert(is_dynamic(A)).\n" + 
				"multifile(A) :- is_multifile(A),!.\n" +
				"multifile(A) :- assert(is_multifile(A)).\n"
				);
    	engine.addTheory(theory);
		
    	theory = new Theory(
    			":- dynamic is_dynamic/1.\n"+
				":- dynamic is_multifile/1.\n");
		engine.addTheory(theory);
		engine.loadLibrary("compatiblitySWI.pl");
		engine.loadLibrary("compatiblityTU.pl");
	*/
	}

	
	public PrologSession getSession_impl() throws Throwable {
		ReusableSocket socket = null;
		TuPrologSession s = new TuPrologSession(this); 
		s.setDispatcher(new PrologInterfaceListener() {
			public void update(PrologInterfaceEvent e) {
				fireUpdate(e.getSubject(), e.getEvent());
			}
		});
			return s;
	}


	/**
	 * @param standAloneServer
	 *            The standAloneServer to set.
	 */
	public void setStandAloneServer(boolean standAloneServer) {
		if (isDown()) {
			this.standAloneServer = standAloneServer;
		} else {
			throw new IllegalStateException(
					"Cannot change standalone flag while in use.");
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#setOption(java.lang.String,
	 *      java.lang.String)
	 */
	public void setOption(String opt, String value) {
		if(WARNING.equals(opt)){
		      setWarning(Boolean.valueOf(value).booleanValue());
		}else if (TIMEOUT.equals(opt)) {
			this.timeout = Integer.parseInt(value);
		} else if (USE_POOL.equals(opt)) {
			//setUseSessionPooling(Boolean.valueOf(value).booleanValue());
		} else {
			throw new IllegalArgumentException("option not supported: " + opt);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#getOption(java.lang.String)
	 */
	public String getOption(String opt) {
		// ld: changed semantic:: System properties override any settings
		String s = System.getProperty(opt);
		if (s != null) {
			Debug.warning("option " + opt
					+ " is overridden by System Property: " + s);
			return s;
		}
		if(WARNING.equals(opt)){
		      return "" + engine.isWarning();
		}else if (STANDALONE.equals(opt)) {
			return "" + standAloneServer;
		} else if (USE_POOL.equals(opt)) {
			return "" + useSessionPooling;
		} else if (TIMEOUT.equals(opt)) {
			return "" + timeout;
		} else {
			throw new IllegalArgumentException("option not supported: " + opt);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeInitialSession(org.cs3.pl.prolog.PrologSession)
	 */
	protected void disposeInitialSession(PrologSession initSession) {
		InitSession s = (InitSession) initSession;
		s.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#disposeShutdownSession(org.cs3.pl.prolog.PrologSession)
	 */
	protected void disposeShutdownSession(PrologSession s) {
		ShutdownSession ss = (ShutdownSession) s;
		ss.doDispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getInitialSession()
	 */
	protected PrologSession getInitialSession() throws PrologInterfaceException {
		try {
			return new InitSession(this);
		} catch (Throwable e) {
			handleException(e);
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#getShutdownSession()
	 */
	protected PrologSession getShutdownSession() throws PrologInterfaceException {
		try {
			return new ShutdownSession(this);
		} catch (Throwable e) {
			handleException(e);
			return null;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.IPrologInterface#getFactory()
	 */
	public PrologInterfaceFactory getFactory() {
		return factory;
	}

	/**
	 * @return Returns the locator.
	 */
	public ResourceFileLocator getLocator() {
		return locator;
	}

	/**
	 * @param locator
	 *            The locator to set.
	 */
	public void setLocator(ResourceFileLocator locator) {
		this.locator = locator;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
	 */
	public synchronized void stop() throws PrologInterfaceException {
		super.stop();

	}

	public synchronized void emergencyStop() {

		super.emergencyStop();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.internal.AbstractPrologInterface#stop()
	 */
	public synchronized void start() throws PrologInterfaceException {
		super.start();
	}

	public void setLockFile(File string) {
		this.lockFile = string;

	}

	public File getLockFile() {
		return lockFile;
	}

	public int getPort() {
		return port;
	}

	public int getTimeout() {
		return timeout;
	}

	public boolean isHidePlwin() {
		return hidePlwin;
	}

	public void setHidePlwin(boolean hidePlwin) {
		this.hidePlwin = hidePlwin;
	}

	public TuProlog getEngine() {
		return engine;
	}

	/**
	 * @see Prolog#addWarningListener(WarningListener)
	 * @param listener
	 */
	public void addWarningListener(WarningListener listener) {
		engine.addWarningListener(listener);	
	}
	
	/**
	 * @see Prolog#addWarningListener(WarningListener)
	 * @param listener
	 */
	public void setWarning(boolean warning) {
		engine.setWarning(warning);	
	}
}