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


/**
 * hook into the PIFs lifecycle.
 */
public interface LifeCycleHook{
    /**
     * called by the PrologInterface during startup phase.
     * <br>
     * This method executes on the same thread that starts the prolog interface.
     * No interaction via regular PrologSessions will take place before
     * this method is called on all registered LifeCycleHooks.
     * <br><b>Important Note:</b> Only access the pif through the
     * initSession argument. In particular, take care that this method
     * does not indirectly trigger a call to PrologInterface.getSession() on the same thread,
     * or you will very propably couse a dead lock. The initial session cannot be 
     * disposed.  
     * @param initSession safe-mode session for startup phase.
     */
	abstract void onInit(PrologInterface pif, PrologSession initSession) throws PrologInterfaceException;
	
	/**
     * called by the PrologInterface  after the startup is complete.
     * <br>
     * This method executes asynchronously to the thread that started
     * the prolog interface. By the time it is called, the pif is guaranteed to be
     * up and ready for normal operation (getSession() and friends).
     * <br>
     * 
     */	
	abstract void afterInit(PrologInterface pif) throws PrologInterfaceException;
	
	/**
     * called by the PrologInterface  before the pif shuts down.
     * <br>
     * This method is called on the same thread that stops the prolog interface.
     * There are no other sessions running. (FIXME verify this!!)
     * <br><b>Important Note:</b> Only access the pif through the
     * initSession argument. In particular, take care that this method
     * does not indirectly trigger a call to PrologInterface.getSession() on the same thread,
     * or you will very propably couse a dead lock. The cleanup session cannot be 
     * disposed.  
     */		
	abstract void beforeShutdown(PrologInterface pif,PrologSession session) throws PrologInterfaceException;	
	
	/**
     * called by the PrologInterface  when it encounters a fatal error.
     * <br>
     * This hook method is called when the PrologInterface detects any kind of problem that
     * keeps it from further communicating with the Prolog process. It will call the 
     * method on all registered hooks in no particular order (dependencies between hooks are 
     * ignored) and will then shutdown.
     * 
     * Note that his hook is called in a state where there is no more connection to the 
     * prolog process. You cannot use any prolog session in this state.
     * 
     * If, when exactly, and under which conditions this hook is called depends on the 
     * implementation. The only guarantee made is that when calling this hook, the 
     * PrologInterface has left its normal life cycle. Before the hook is called, the 
     * PrologInterface is in sate ERROR, after they have been called, it will enter
     * state DOWN. No other hook methods will be called in between.
     */		
	public void onError(PrologInterface pif);
	
	/**
	 * parameterize this hook instance with domain data.
	 */
	public void setData(Object data);
	
	/**
	 * called by the PrologInterface when the hook is registered while the pif is already up or in the process of starting up.
	 * When it is called, the PrologInterface is up and running.
	 * Note that this method will ignore hook dependencies. 	
	 * 
	 * Most implementation will just call onInit and/or after init, since they will not 
	 * be called by the PrologInterface when the hook is registered "to late", i.e. after the
	 * startup sequence has begun.
	 * 
	 */
	public void lateInit(PrologInterface pif);

}
