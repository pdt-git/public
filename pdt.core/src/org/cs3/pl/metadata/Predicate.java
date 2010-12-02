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
 * A handle Prolog predicate.
 * 
 * An instance of this class is a handle to a prolog predicate.
 * 
 * Two predicates are considered equal, if there module, name and arity are equal.
 * 
 * An instance is really not much more than a name to identify a predicate, e.g. when
 * passing it arround as argument to method calls.
 * 
 * Other properties besides module, name and arity may be attached to an instance. 
 * Predefined property names are EXPORTED, MULTIFILE, DYNAMIC, MODULE_TRANSPARENT.
 * 
 * Implementations are not required to attach this information. 
 * Implementations are allowed to attach any other information. However, the predefined
 * property keys may only be used with the meaning declared in the description of the
 * respective key.    
 *  
 *  
 * @author lukas
 *
 */
public interface Predicate {

	/**
	 * predicate property exported.
	 * 
	 * May be used to indicate wether this predicate is exported, i.e. its signatrue 
	 * occurs in the definition module's export list.
	 * 
	 * Valid values are "true" if this predicate is exported, "false" if it is not exported.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String EXPORTED = "exported";
	
	/**
	 * predicate property multifile.
	 * 
	 * May be used to indicate wether this predicate is a multifile predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin multifile/1
	 * 
	 * Valid values are "true" if this predicate is a multifile predicate, 
	 * "false" if it is not a multifile predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String MULTIFILE = "multifile";
	
	/**
	 * predicate property dynamic.
	 * 
	 * May be used to indicate wether this predicate is a dynamic predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin dynamic/1
	 * 
	 * Valid values are "true" if this predicate is a dynamic predicate, 
	 * "false" if it is not a dynamic predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */
	public final static String DYNAMIC = "dynamic";
	
	/**
	 * predicate property module_transparent.
	 * 
	 * May be used to indicate wether this predicate is a module-transparent predicate, i.e. 
	 * its signatrue apears in a directive as argument to the builtin module_transparent/1
	 * 
	 * Valid values are "true" if this predicate is a module transparent predicate, 
	 * "false" if it is not a module transparent predicate.
	 * null means absence of knowledge, i.e. either may be true.
	 */	
	public final static String MODULE_TRANSPARENT = "module_transparent";
	
	
	/**
	 * 
	 * @param the property name.
	 * @return the property value or null if the property is not set.
	 */
	public String getPredicateProperty(String property);
	
	public void setPredicateProperty(String property, String value);
	
	/**
	 *  
	 * @return the signature of the predicate:
	 * name/arity.
	 */
	public String getSignature();

	/**
	 * get the predicate name.
	 * 
	 *  really only the name atom. No arity, no module.
	 *  e.g. for the builtin system:current_thread/2, this method 
	 *  returns "current_thread" 
	 * 
	 * @return the predicate name.
	 */
	public String getName();
	
	/**
	 * @return the arity, i.e. number of arguments of this predicate.
	 */
	public int getArity();

	/**
	 * @return the definition module of this predicate.
	 */
	public String getModule();
	
	public boolean isDynamic();

	public boolean isMultifile();

	public boolean isPublic();

}