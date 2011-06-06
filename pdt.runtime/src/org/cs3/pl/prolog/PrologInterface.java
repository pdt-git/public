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

package org.cs3.pl.prolog;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.runtime.BootstrapPrologContribution;
import org.cs3.pl.common.PreferenceProvider;

public interface PrologInterface {

	/**
	 * consult event subject constant events of this subject will be fired
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
	 * 
	 * session flag.
	 * This is what will be used by the legacy PrologInterface.getSession()
	 * method.
	 */
	public final static int LEGACY = UNQUOTE_ATOMS | PROCESS_LISTS;
	
	/**
	 * property constant.
	 */
	public static final String PREF_FILE_SEARCH_PATH = "pif.file_search_path";
	public static final String PREF_EXECUTABLE = "pif.executable";
	public static final String PREF_ENVIRONMENT = "pif.environment";
	public static final String PREF_STANDALONE = "pif.standalone";
	public static final String PREF_TIMEOUT = "pif.timeout";
	public static final String PREF_HOST = "pif.host";
	

	/**
	 * Returns a prolog session.<br>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologInterface is in UP state. During startup, this
	 * call will block until the pif is up. In state SHUTODWN or DOWN, this will
	 * raise an IllegalStateException.
	 * 
	 * Uses flag=LEGACY
	 * 
	 * @return a new Session Object
	 */
	public abstract PrologSession getSession() throws PrologInterfaceException;

	/**
	 * Returns a prolog session.<br>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologInterface is in UP state. During startup, this
	 * call will block until the pif is up. in state SHUTODWN or DOWN, this will
	 * raise an IllegalStateException.
	 * 
	 * Flag sets the kind of objects returned by the queries.
	 * 
	 * @return a new Session Object
	 */
	public abstract PrologSession getSession(int flags) throws PrologInterfaceException;
	
	/**
	 * Stop the prolog system (if it is up). This will terminate all running
	 * sessions and shut down the prolog process.
	 * 
	 * @throws IOException
	 */
	public abstract void stop() throws PrologInterfaceException;

	/**
	 * Starts the prolog system (if it is down).
	 * 
	 * @throws IOException
	 */
	public abstract void start() throws PrologInterfaceException;

	public abstract void restart() throws PrologInterfaceException;

	public abstract void reset() throws PrologInterfaceException;

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
	 * 
	 * @see PrologInterfaceFactory.getOptions()
	 */
	public void initOptions(PreferenceProvider provider);	
	
	public void setStandAloneServer(boolean standAloneServer);

	public boolean isStandAloneServer();
	public String getExecutable();
	public void setExecutable(String executable);
	public String getEnvironment() ;
	public void setEnvironment(String executable) ;
	public String getHost();
	public void setHost(String host);
	public String getFileSearchPath();
	public int getTimeout();
	public void setTimeout(String timeout);
	public void setFileSearchPath(String fileSearchPath);
	

	/**
	 * get the life list of bootstrap libraries. <br>
	 * "life" means, that any modification will affect the next startup of the
	 * pif. The list contains path strings (the "prolog kind" of paths) to
	 * prolog files that will be consulted during startup of the pif.
	 * 
	 * @return the life list of bootstrap libraries
	 */
	public List<BootstrapPrologContribution> getBootstrapLibraries();

	/**
	 * @see getBootStrapLibraries()
	 * @param l
	 */
	public void setBootstrapLibraries(List<BootstrapPrologContribution> l);


	/**
	 * unregister a lifeCycleHook.
	 * 
	 * this will remove ALL hooks registered for this id.
	 * 
	 * @param reconfigureHookId
	 */
	public abstract void removeLifeCycleHook(String hookId);
	public void removeLifeCycleHook(final LifeCycleHook hook,final String hookId);
	
	/**
	 * Uses the PrologInterface.LEGACY 
	 */
	public AsyncPrologSession getAsyncSession() throws PrologInterfaceException;
	public AsyncPrologSession getAsyncSession(int flags) throws PrologInterfaceException;
	
	/**
	 * Is the {@link PrologInterface} in an error state, e.g. the corresponding process has been killed externally.  
	 * @return
	 */
	public boolean hasError();
	
	public List<Map<String, Object>> queryAll(String... predicates) throws PrologInterfaceException;
	
	public Map<String, Object> queryOnce(String... predicates) throws PrologInterfaceException;

}