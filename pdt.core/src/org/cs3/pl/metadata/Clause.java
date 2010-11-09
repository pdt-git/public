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


/**
 * A prolog clause handle.
 * 
 * An instance of this class serves as a handle to a piece of 
 * prolog code. Note that instances are not related to runtime clauses/clause references 
 * that are subject to builtins clause/3 or clause_property/2. Instead, instaces of this class
 * are handles to entities of the source code model.
 * 
 * In particular, clauses asserted at runtime are not instances of this class.
 * A clause is always defined in some well known file that is persisted on a filesystem
 * A clause has a well defined position within this file, an interval of characters that
 * 	- contains the definition of this clause, 
 *  - does not intersect the definition of any other 
 *  - member or comment and does not contain any leading or trailing whitespace.
 *  
 * Both, the file aswell as the definition of the file can be obtained using the member
 * method getSourceLocation(). Implementations that fail to provide this inforamtion are in 
 * violation with the contract of this interface. 
 * 
 * Two clauses are considered equal if there their source location property is equal.
 * @author lukas
 *
 */
public interface Clause {


	/**
	 * @return the location in the source code where this clause is 
	 * defined.
	 */
	public SourceLocation getSourceLocation();
		
	public String getName();
	
	public int getLength();

	public int getArity();
	
	/**
	 * @return a handle to the Predicate this clause contributes to.
	 */
	public Predicate getPredicate();

}