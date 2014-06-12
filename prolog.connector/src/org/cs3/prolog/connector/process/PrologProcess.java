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

package org.cs3.prolog.connector.process;

import java.io.File;
import java.util.List;
import java.util.Map;

import org.cs3.prolog.connector.common.PreferenceProvider;
import org.cs3.prolog.connector.lifecycle.LifeCycleHook;
import org.cs3.prolog.connector.session.AsyncPrologSession;
import org.cs3.prolog.connector.session.PrologSession;

public interface PrologProcess {

	/**
	 * consult event subject constant.
	 * Events of this subject will be fired
	 * whenever something was consulted into the prolog system. <br>
	 * NOT IMPLEMENTED YET
	 */
	public final static String SUBJECT_CONSULTED = "consulted";

	

	/**
	 * session flag.
	 * 
	 * this shall eventually be the *new* default behavior. All bindings are
	 * reported as java.lang.String objects using the canonical syntax. Atoms
	 * are quoted when necessary. lists are not processed. I.e. all bindings
	 * should be of a form as created by write_canonical/1.
	 * 
	 */
	public final static int NONE = 0;

	/**
	 * session flag.
	 * 
	 * Deviates from NONE in that bindings that are atoms are unquoted. This is
	 * supposed to mimic the "old" behavior where bindings where written into
	 * the stream using write/2 rather than write_canonical/2 or writeq/2. Note
	 * that this will NOT un-quote atoms nested in complex terms, so the
	 * behavior is slightly different than it was before.
	 */
	public final static int UNQUOTE_ATOMS = 1;

	/**
	 * session flag.
	 * 
	 * Deviates from NONE in that bindings that are lists are reported as
	 * java.util.List instances. Elements are processed recursively.
	 */
	public final static int PROCESS_LISTS = 2;

	/**
	 * session flag.
	 * 
	 * Deviates from NONE in that all bindings are reported as instances of
	 * org.cs3.pl.cterm.CTerm. Cannot be used together with UNQUOTE_ATOMS. Doing
	 * so will raise an IllegalArgumentException. Can be combined with
	 * PROCESS_LISTS.
	 * 
	 */
	public final static int CTERMS = 4;

	
	/**
	 * session flag.
	 * 
	 * If this flag is set, all variables will be part of the result, even
	 * the variables which are not bound (you will have entries like A=A)
	 * 
	 */
	public final static int UNBOUND_VARIABLES = 8;
	
	/**
	 * 
	 * session flag.
	 * 
	 * This is what will be used by the legacy PrologProcess.getSession()
	 * method.
	 */
	public final static int LEGACY = UNQUOTE_ATOMS | PROCESS_LISTS;
	
	/**
	 * 
	 * session flag.
	 * 
	 * This is what should be used by JPC. It creates CTerms and create
	 * result entries for unbound variables.
	 * 
	 */
	public final static int JPC = CTERMS | UNBOUND_VARIABLES;
	
	
	

	/**
	 * Returns a prolog session.<br>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologProcess is in UP state. During startup, this
	 * call will block until the process is up. In state SHUTODWN or DOWN, this will
	 * raise an IllegalStateException.
	 * 
	 * Uses default flag
	 * 
	 * @return a new Session Object
	 * @throws PrologProcessException
	 */
	public abstract PrologSession getSession() throws PrologProcessException;

	/**
	 * Returns a prolog session.<br>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologProcess is in UP state. During startup, this
	 * call will block until the process is up. in state SHUTODWN or DOWN, this will
	 * raise an IllegalStateException.
	 * 
	 * Flag sets the kind of objects returned by the queries.
	 * 
	 * @return a new Session Object
	 * @throws PrologProcessException
	 */
	public abstract PrologSession getSession(int flags) throws PrologProcessException;
	
	/**
	 * Stop the prolog system (if it is up). This will terminate all running
	 * sessions and shut down the prolog process.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void stop() throws PrologProcessException;

	/**
	 * Starts the prolog system (if it is down).
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void start() throws PrologProcessException;

	/**
	 * Restarts the prolog system.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void restart() throws PrologProcessException;

	/**
	 * Stops and resets the prolog system.
	 * 
	 * @throws PrologProcessException
	 */
	public abstract void reset() throws PrologProcessException;

	/**
	 * checks whether the prologInterface is up and running.
	 * 
	 * @return true if the prolog system is ready for battle.
	 */
	public boolean isUp();

	/**
	 * checks whether the prologInterface is down. <br>
	 * this is not the same as <code>!isUp()</code>. During startup and
	 * shutdown both methods return false.
	 * 
	 * @return
	 */
	public boolean isDown();

	public void addLifeCycleHook(LifeCycleHook hook, String id,
			String[] dependencies);

	/**
	 * initializes options of this prolog interface from preference_store
	 */
	public void initOptions(PreferenceProvider provider);	
	
	public void setStandAloneServer(boolean standAloneServer);

	public boolean isStandAloneServer();
	public String getOSInvocation();
	public void setOSInvocation(String osInvocation);
	public String getExecutablePath();
	public void setExecutablePath(String executablePath);
	public String getCommandLineArguments();
	public void setCommandLineArguments(String commandLineArguments);
	public String getAdditionalStartupFile();
	public void setAdditionalStartupFile(String additionalStartupFile);
	public String getEnvironment() ;
	public void setEnvironment(String executable) ;
	public String getHost();
	public void setHost(String host);
	public String getFileSearchPath();
	public int getTimeout();
	public void setTimeout(String timeout);
	public Object getAttribute(String attribute);
	public void setAttribute(String attribute, Object value);
	public void setFileSearchPath(String fileSearchPath);
	
	
	public StartupStrategy getStartupStrategy();
	public void setStartupStrategy(StartupStrategy startupStrategy);

	/**
	 * unregister a lifeCycleHook.
	 * 
	 * this will remove ALL hooks registered for this id.
	 * 
	 * @param hookId
	 */
	public abstract void removeLifeCycleHook(String hookId);
	public void removeLifeCycleHook(final LifeCycleHook hook,final String hookId);
	
	/**
	 * Uses the default flag
	 */
	public AsyncPrologSession getAsyncSession() throws PrologProcessException;
	public AsyncPrologSession getAsyncSession(int flags) throws PrologProcessException;
	
	/**
	 * Is the {@link PrologProcess} in an error state, e.g. the corresponding process has been killed externally.  
	 * @return
	 */
	public boolean hasError();
	
	/**
	 * Executes the given query and returns all results. The query is created
	 * by connecting the given goals conjunctively. The result is always a list of maps. Each map
	 * represents one result of the query containing the bindings for all variables. 
	 * The variables are the keys of each map.
	 * If the query fails the returned list is empty.
	 * 
	 * Uses default flag
	 * 
	 * @param predicates a number of goals
	 * @return all results of the query or an empty list if the query fails
	 * @throws PrologProcessException
	 */
	public List<Map<String, Object>> queryAll(String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns all results. The query is created
	 * by connecting the given goals conjunctively. The result is always a list of maps. Each map
	 * represents one result of the query containing the bindings for all variables. 
	 * The variables are the keys of each map.
	 * If the query fails the returned list is empty.
	 * 
	 * Flag sets the kind of objects returned by the query.
	 * 
	 * @param flag kind of objects returned by the query
	 * @param predicates a number of goals
	 * @return all results of the query or an empty list if the query fails
	 * @throws PrologProcessException
	 */
	public List<Map<String, Object>> queryAll(int flag, String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns the first result. The query is created
	 * by connecting the given goals conjunctively. If the query succeeds, the result is a map
	 * containing the bindings for all variables. The variables are the keys of the map.
	 * If the query fails this method returns null.
	 * 
	 * Uses default flag
	 * 
	 * @param predicates a number of goals
	 * @return the first result as of the query or null if the query fails
	 * @throws PrologProcessException
	 */
	public Map<String, Object> queryOnce(String... predicates) throws PrologProcessException;
	
	/**
	 * Executes the given query and returns the first result. The query is created
	 * by connecting the given goals conjunctively. If the query succeeds, the result is a map
	 * containing the bindings for all variables. The variables are the keys of the map.
	 * If the query fails this method returns null.
	 * 
	 * Flag sets the kind of objects returned by the query.
	 * 
	 * @param flag kind of objects returned by the query
	 * @param predicates a number of goals
	 * @return the first result as of the query or null if the query fails
	 * @throws PrologProcessException
	 */
	public Map<String, Object> queryOnce(int flag, String... predicates) throws PrologProcessException;
	
	public int getDefaultSessionFlag();

	public void setDefaultSessionFlag(int flag);
	
	public void consult(File file) throws PrologProcessException;
}


