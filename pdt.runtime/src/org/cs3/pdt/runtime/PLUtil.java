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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.internal.ATermFactory;
import org.cs3.pl.prolog.PrologSession;

/**
 * some frequently used code fragments related to the setup of prolog runtimes.
 * 
 */
public class PLUtil {

	public static void configureFileSearchPath(PrologLibraryManager mgr,
			PrologSession session, String[] libIds) {

		StringBuffer sb = new StringBuffer();
		PrologLibrary[] required = getRequiredLibs(mgr, libIds);
		for (int i = 0; i < required.length; i++) {
			PrologLibrary lib = required[i];

			if (sb.length() > 0) {
				sb.append(',');
			}
			sb.append("(	user:file_search_path(" + lib.getAlias() + ", '"
					+ lib.getPath() + "')" + "->	true"
					+ ";	user:assert(file_search_path(" + lib.getAlias()
					+ ", '" + lib.getPath() + "'))" + ")");
		}

		session.queryOnce(sb.toString());

	}

	public static PrologLibrary[] getRequiredLibs(PrologLibraryManager mgr,
			String[] libIds) {

		Stack todo = new Stack();
		Set required = new HashSet();
		for (int i = 0; i < libIds.length; i++) {
			if (mgr.resolveLibrary(libIds[i]) == null) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is unresolved");
			}
			if (mgr.getBrokenLibraries().contains(libIds[i])) {
				throw new IllegalArgumentException("library id " + libIds[i]
						+ " is broken");
			}
			todo.add(libIds[i]);

		}

		while (!todo.isEmpty()) {
			String key = (String) todo.pop();
			PrologLibrary lib = mgr.resolveLibrary(key);
			if (lib == null) {
				// this should not happen
				throw new IllegalStateException("unresoved: " + key
						+ ". Bug in LibraryManager?");
			}
			if (!required.contains(lib)) {
				required.add(lib);
				todo.addAll(lib.getDependencies());
			}

		}

		return (PrologLibrary[]) required.toArray(new PrologLibrary[required
				.size()]);
	}

	public static CTerm[] listAsArray(CTerm term) {
		Vector v = new Vector();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			v.add(compound.getArgument(0));
			term = compound.getArgument(1);
		}
		return (CTerm[]) v.toArray(new CTerm[v.size()]);
	}
	
	/**
	 * Converts a property list term to a Map.
	 * 
	 * The argument should be list containing elements of the form property(value).
	 * Atomic elements are interpreted as if they were of the form property(true).
	 * The returned map will contain a single value for each property found.
	 * If the same property name is encountered more than once, the returned map will
	 * reflect only the last occurance.
	 * @see listAsMultiMap(CTerm) TODO
	 * @param term a term with functor "./2"
	 *  
	 * @return a map with keytype string, value type CTerm
	 */
	public static Map listAsMap(CTerm term) {
		Map m = new HashMap();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			CTerm propertyTerm = compound.getArgument(0);
			if(propertyTerm instanceof CCompound && propertyTerm.getArity()==1){
				m.put(propertyTerm.getFunctorValue(), ((CCompound)propertyTerm).getArgument(0));
			}
			else if(propertyTerm.getArity()==0){
				m.put(propertyTerm.getFunctorValue(), "true");
			}
			
			term = compound.getArgument(1);
		}
		return m;
	}
	
	private static ATermFactory factory = new ATermFactory();
	
	/**
	 * @deprecated this is an ad-hoc solution. I am not sure yet where to put this stuff.
	 */
	public static CTerm createCTerm(Object input){
		return factory.createCTerm(input);
	}
}
