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

package org.cs3.pdt.runtime;

import java.util.Iterator;
import java.util.Vector;

/**
 * Convenience class for implementing PrologContextTracker.
 * 
 * Subclasses need to provide implementations for init() and
 * getCurrentPrologInterface(). In addition, subclasses are responsible for
 * calling fireContextChanged() when apropiate, i.e. when they think that the
 * "current" PrologInterface has changed, become available or invalid.
 * 
 * Clients that want to register a static PrologContextTracer instance via the
 * extension point prologContextTracker *MUST* subclass this class.
 * 
 * Other clients are free to use this class as a starting point.
 * 
 * @author lukas
 * 
 */
public abstract class AbstractPrologContextTracker implements
		PrologContextTracker {

	private String id;

	private String label;

	private Vector listeners = new Vector();

	/**
	 * Notify listeners that the "current" PrologInterface has changed.
	 * 
	 * Subclasses should call this method whenever they think the "current"
	 * PrologInterface has changed, become available or invalid.
	 */
	protected void fireContextChanged() {

		PrologContextTrackerEvent e = new PrologContextTrackerEvent(this,
				getCurrentPrologInterface());
		Vector cloned = null;
		synchronized (listeners) {
			cloned = (Vector) listeners.clone();
		}
		for (Iterator it = cloned.iterator(); it.hasNext();) {
			PrologContextTrackerListener l = (PrologContextTrackerListener) it
					.next();
			l.contextChanged(e);
		}
	}

	public void addPrologContextTrackerListener(PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologContextTrackerListener(
			PrologContextTrackerListener l) {

		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}

	}

	public String getLabel() {
		return label;
	}

	public String getId() {
		return id;
	}

	public AbstractPrologContextTracker() {
		id = null;
		label = null;
	}

	public AbstractPrologContextTracker(String id, String label) {
		this.id = id;
		this.label = label;
	}

	public void setLabel(String label) {
		this.label = label;

	}

	public void setId(String id) {
		this.id = id;
	}

}
