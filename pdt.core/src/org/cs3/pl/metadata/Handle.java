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


package org.cs3.pl.metadata;

import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.PrologInterfaceException;


/**
 * A handle to a pdt.core entity.
 * 
 * Handles should be used to refer to any kind of pdt.core entity.
 * For any handle instance, a prolog term can be constructed that can be used 
 * as an argument in prolog api calls and that refers to the same entity.
 * 
 *  A note on handle identity:
 *  Two handles should be considered equal if and only if they refer to the same 
 *  entity. If there are two handles with conflicting cached properties that refer to the
 *  same entity, than at least one of them is outdated. If such a situation is encountered,
 *  the method lookup() should be called on both entities, which will either throw an exception
 *  (if one of the handles is invalid) or update the property cache.
 */

public interface Handle {
	/**
	 * construct a pdt core entity handle.
	 * 
	 * entity handles a terms of the form handle(Id,Type,Cache)
	 * This method will provide a type and an Id, such that the pdt core
	 * can lookup the underlying entity and provide properties for it.
	 * The third argument of the handle term *MUST* be an unbound variable named 
	 * "Cache". 
	 * 
	 * Note that legazy implemantations may not always be able to provide this information. 
	 * 
	 * 
	 * @return the handle or null if no handle can be constructed.
	 */
	public String constructHandleTerm();
	
	/**
	 * lookup the entity in the corresponding entity index.
	 * This will update the handles property cache.
	 * @throws PrologInterfaceException 
	 * @throws InvalidHandleException if the lookup fails.
	 */
	public void lookup() throws InvalidHandleExcpetion, PrologInterfaceException;
	
	/**
	 * add an arbitrary property->value binding to the property cache of this handle.
	 * This does NOT alter the state of the underlying entity.
	 */
	public void setCachedProperty(String name, CTerm value);
	
	/**
	 * retrieve a value bound to a property in the handles property cache.
	 * This does NOT query the state of the underlying entity.
	 */
	public CTerm getCachedProperty(String name);
	
	/**
	 * Add a listener to the list of registered EntityListeners.
	 * 
	 * Listeners will be informed if the underlying entity is updated or removed.
	 * THIS IS OPTIONAL BEHAVIOUR: wether or not notifications are actually created
	 * is up to the prolog side implementation of the entity backend.
	 * @throws PrologInterfaceException 
	 * 
	 */
	public void addEntityListener(EntityListener l) throws PrologInterfaceException;
	
	/**
	 * Remove a listener from the list of registered EntityListeners
	 * @param l
	 * @throws PrologInterfaceException 
	 */
	public void removeEntityListener(EntityListener l) throws PrologInterfaceException;
}
