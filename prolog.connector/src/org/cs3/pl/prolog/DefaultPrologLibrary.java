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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;


public class DefaultPrologLibrary implements PrologLibrary {

	private String id;
	private Set<String> deps;
	private String alias;
	private String path;
	private HashMap<String, String> attributes= new HashMap<String, String>();

	public DefaultPrologLibrary(String id, String[] deps, String alias, String path) {
		super();
		this.id = id;
		this.deps=new HashSet<String>();
		for (int i = 0; i < deps.length; i++) {
			this.deps.add(deps[i]);
		}		
		this.alias = alias;
		this.path = path;
	}
	
	public DefaultPrologLibrary(String id, Set<String> deps, String alias, String path) {
		super();
		this.id = id;
		this.deps = deps;
		this.alias = alias;
		this.path = path;
	}

	public DefaultPrologLibrary(String id, Set<String> deps, String alias, String path, Map<String, String> libAttrs) {
		this(id,deps,alias,path);
		this.attributes.putAll(libAttrs);
	}

	@Override
	public String getId() {		
		return this.id;
	}

	@Override
	public String getPath() {
		return this.path;
	}

	@Override
	public String getAlias() {
		return this.alias;
	}

	@Override
	public Set<String> getDependencies() {
		return this.deps;
	}

	@Override
	public String getAttributeValue(String attr) {
		return attributes.get(attr);
	}

}
