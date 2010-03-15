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

package org.cs3.pdt.runtime.ui;

import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.ui.IWorkbench;

/**
 * keeps track of "the current" PrologInterface.
 * 
 * As there might be several (conflicting) notions or strategies of determining
 * the "active" or "current" PrologInterface at a given point in time, I decided
 * to attach a name to each of this strategies and let the user decide which one
 * to use.
 * 
 * Clients who are interested in finding out or following the currently "active"
 * PrologInterface should check with the ContextTrackerService to obtain a list
 * of all contributed strategies and either choose one that seems fitting or
 * even beter: ask the user.
 * 
 * Clients can overide this Interface to contribute their own
 * PrologContextTracker strategy and make it available to other clients by
 * registering it at the PrologRuntimePlugin's ContextTrackerService. Typically
 * this is done by extending the extension point prologContextTracker of the
 * pdt.runtime plugin. 
 * 
 * @author lukas
 * 
 */
public interface PrologContextTracker {
	/**
	 * add a listener.
	 * 
	 * Note: In most cases, listeners should be registered with the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * The listener will be informed whenever the tracker thinks that the active
	 * PrologInterface has changed, becomes available or invalid.
	 * 	 * 
	 * Implemntations are responsible for calling the listener methods.
	 * 
	 * 
	 * @param l
	 */
	public void addPrologContextTrackerListener(PrologContextTrackerListener l);

	/**
	 * remove a listener.
	 * 
	 * Note: In most cases, listeners should be unregistered from the
	 * PrologContextTrackerService instead. See there for details.
	 * 
	 * @param l
	 */
	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l);

	/**
	 * @return a descriptive label for this tracker. This will be presented to
	 *         the user.
	 */
	public String getLabel();

	/**
	 * @return a unique identifier for this tracker.
	 */
	public String getId();

	/**
	 * There is no current PrologInterface.
	 * find out what this tracker THINKS is the currently active PrologInterface.
	 * 
	 * @return the PrologInterface or null, if the tracker thinks that none is
	 *         active.
	 * @throws PrologInterfaceException 
	 * @throws PrologInterfaceException 
	 */
	public PrologInterface getCurrentPrologInterface() ;

	/**
	 * initialize the tracker.
	 * 
	 * This method is called by the pdt.runtime plugin at the earliest possible
	 * time that satisfies the following conditions: - the tracker has been
	 * registered with the plugin's tracker service - the eclipse workbench ui
	 * is up and running.
	 * 
	 * Implementation should seize the oportunity to attach to parts of the
	 * workbench ui they might be interested in.
	 * 
	 * @param workbench
	 */
	public void init(IWorkbench workbench);
}
