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

import java.util.Set;

/**
 * A contribution to the Prolog file lookup path.
 * 
 * An instance moreless represents a clause that can be
 * contributed to the file_search_path/2 predicate.
 * @author lukas
 *
 */
public interface PrologLibrary {
	/**
	 * @return the global unique identifier for this library
	 */
	public String getId();
	
	/**
	 * 
	 * @return The resolved absolute path (the prolog flavour) of the 
	 * directory that contains the library
	 */
	public String getPath();
	
	/**
	 * @return The alias this library uses. (see file_search_path/2)
	 */
	public String getAlias();
	
	/**
	 * @return a list of identifiers of libraries this library depends on.
	 */
	public Set getDependencies();
	
	/**
	 * retrieve application-specific data from the library.
	 * 
	 * Clients may attach additional, string valued attributes to the library that are relevant 
	 * to the respective application.
	 * The pdt core for example adds a flag "hidden" which is set for all and runtime libraries.
	 *  
	 * @param attr the attribute name
	 * @return the attribute value or null if not set.
	 */
	public String getAttributeValue(String attr);
}
