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

import java.util.Map;

/**
 * A Subscription that can be persisted.
 * 
 * PersistableSubscriptions can store/restore their complete state to/from a
 * key->value map. It is the responsibility of the implementing class to supply
 * adaequat implementations to saveState() and restoreState(Map).
 * 
 * implementing classes MUST be default-constructable.
 * 
 * @author lukas
 * 
 */
public interface PersistableSubscription extends Subscription {
	/**
	 * retrieve the bundle id of the hosting plugin.
	 * 
	 * when the subscription is restored, the host bundle needs to be known, so
	 * the correct classloader is used.
	 * 
	 * @return the bundle id of the plugin hosting the subscription.
	 */
	public abstract String getHostId();

	/**
	 * Restore from saved state.
	 * 
	 * This is called by the registry. Implementations should use this method to
	 * completely initialize their state according to the given parameter
	 * values. The framework assumes that the subscription is completely
	 * initialized after this call.
	 * 
	 * @param params
	 *            a map of parameternames and respective values. Key and value
	 *            type is String. This map contains the same data as the on that
	 *            was previously returned by saveState.
	 */
	public abstract void restoreState(Map params);

	/**
	 * save state.
	 * 
	 * This method is called by the framework before shutdown. Implementations
	 * should return a (modifyable!) map of parameter names and respective
	 * values. Key and value type MUST be String. The map should contain enough
	 * information to completely restore the state by calling restoreState(Map)
	 * on an uninitialized Instance.
	 * 
	 * @return the saved state.
	 */
	public abstract Map saveState();

	/**
	 * Check wether this subscription should be persisted on shutdown.
	 * 
	 * @return if true, the framework will save the state on shutdown using
	 *         saveState() and restore it at next startup using restoreState().
	 */
	public abstract boolean isPersistent();
}
