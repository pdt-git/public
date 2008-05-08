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

public interface PrologInterface {

	/**
	 * consult event subject constant events of this subject will be fired
	 * whenver something was consulted into the prolog system. <br>
	 * NOT IMPLEMENTED YET
	 */
	public final static String SUBJECT_CONSULTED = "consulted";

	/**
	 * property constant.
	 */
	public static final String FILE_SEARCH_PATH = "pif.file_search_path";

	/**
	 * session flag.
	 * 
	 * this shall eventually be the *new* default behaviour. All bindings are
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
	 * supposed to mimic the "old" behaviour where bindings where written into
	 * the stream using write/2 rather than write_canonical/2 or writeq/2. Note
	 * that this will NOT unquote atoms nested in complex terms, so the
	 * behaviour is slightly different than it was before.
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
	 * Returns a prolog session.<br>
	 * Use sessions to interact with the prolog system. Sessions can only be
	 * obtained while the PrologInterface is in UP state. During startup, this
	 * call will block until the pif is up. in state SHUTODWN or DOWN, this will
	 * raise an IllegalStateException.
	 * 
	 * @return a new Session Object
	 * @deprecated use getSession(int).
	 */
	public abstract PrologSession getSession() throws PrologInterfaceException;

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
	 * set a configuration option of this prolog interface.
	 * 
	 * @see PrologInterfaceFactory.getOptions()
	 */
	public void setOption(String opt, String value);

	/**
	 * get the current value of a configuration option.
	 * 
	 * @see PrologInterfaceFactory.getOptions()
	 */
	public String getOption(String opt);

	/**
	 * get the life list of bootstrap libraries. <br>
	 * "life" means, that any modification will affect the next startup of the
	 * pif. The list contains path strings (the "prolog kind" of paths) to
	 * prolog files that will be consulted during startup of the pif.
	 * 
	 * @return the life list of bootstrap libraries
	 */
	public List<String> getBootstrapLibraries();

	/**
	 * @see getBootStrapLibraries()
	 * @param l
	 */
	public void setBootstrapLibraries(List<String> l);

	/**
	 * 
	 * @return the factory instance that created this pif, or if this pif was
	 *         not created by a factory (unlikely :-) ).
	 */
	public PrologInterfaceFactory getFactory();

	/**
	 * unregister a lifeCycleHook.
	 * 
	 * this will remove ALL hooks registered for this id.
	 * 
	 * @param reconfigureHookId
	 * @deprecated If possible please use
	 *             PrologInterface2.removeLifeCycleHook(LifeCycleHook2,String)
	 */
	public abstract void removeLifeCycleHook(String hookId);

	

}