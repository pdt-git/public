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

import java.io.File;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Map;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CNil;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.CVariable;
import org.cs3.pl.cterm.internal.ATermFactory;
import org.cs3.pl.cterm.internal.ParserCTermFactory;

/**
 * some frequently used code fragments related to the setup of prolog runtimes.
 * 
 */
public class PLUtil {

	public static void configureFileSearchPath(PrologLibraryManager mgr,
			PrologSession session, String[] libIds) throws PrologException,
			PrologInterfaceException {

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
					+ ", '" + lib.getPath() + "'))");
			if ("true".equals(lib.getAttributeValue("hidden"))) {
				sb.append(", pdt_util:assert(pdt_hidden_path('" + lib.getPath()
						+ "'))");
			}
			sb.append(")");
		}

		session.queryOnce(sb.toString());

	}

	public static PrologLibrary[] getRequiredLibs(PrologLibraryManager mgr,
			String[] libIds) {

		Stack<String> todo = new Stack<String>();
		Set<PrologLibrary> required = new HashSet<PrologLibrary>();
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
			String key = todo.pop();
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

		return required.toArray(new PrologLibrary[required
				.size()]);
	}

	public static CTerm[] listAsArray(CTerm term) {
		Vector<CTerm> v = listAsVector(term);
		return (CTerm[]) v.toArray(new CTerm[v.size()]);
	}

	public static Vector<CTerm> listAsVector(CTerm term) {
		Vector<CTerm> v = new Vector<CTerm>();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			v.add(compound.getArgument(0));
			term = compound.getArgument(1);
		}
		return v;
	}

	/**
	 * Converts a property list term to a Map.
	 * 
	 * The argument should be list containing elements of the form
	 * property(value) or =(Property,Value). Atomic elements are interpreted as
	 * if they were of the form property(true). The returned map will contain a
	 * single value for each property found. If the same property name is
	 * encountered more than once, the returned map will reflect only the last
	 * occurance.
	 * 
	 * @see listAsMultiMap(CTerm) TODO
	 * @param term
	 *            a term with functor "./2"
	 * 
	 * @return a map with keytype string, value type CTerm
	 */
	public static Map<String,Object> listAsMap(CTerm term) {
		Map<String,Object> m = new HashMap<String,Object>();
		while (term instanceof CCompound && ".".equals(term.getFunctorValue())
				&& 2 == term.getArity()) {
			CCompound compound = (CCompound) term;
			CTerm propertyTerm = compound.getArgument(0);
			if (propertyTerm instanceof CCompound) {
				if (propertyTerm.getFunctorValue().equals("=")
						&& propertyTerm.getArity() == 2) {
					m.put(
							(renderTerm(((CCompound) propertyTerm)
									.getArgument(0))),
							((CCompound) propertyTerm).getArgument(1));
				} else if (propertyTerm.getArity() == 1) {
					m.put(propertyTerm.getFunctorValue(),
							((CCompound) propertyTerm).getArgument(0));
				}

			} else if (propertyTerm.getArity() == 0) {
				m.put(propertyTerm.getFunctorValue(), "true");
			}

			term = compound.getArgument(1);
		}
		return m;
	}

	/**
	 * lookup an entry in a red-black tree.
	 * 
	 * See module org/cs3/pdt/util/pdt_util_rbtree for details on the expected
	 * datastructure. This method does NOT call prolog. It performs a binary
	 * search on the CTerm data structure passed as first argument.
	 * 
	 * Note that only the functor name of key terms is compared. (I do not want
	 * to implement deep standard-order term comparision in java).
	 * 
	 * @param tree
	 * @param key
	 * @return the first match found or null if none found.
	 * 
	 */
	public static CTerm rbtreeLookup(CTerm tree, String key) {
		while (tree instanceof CCompound) {// if it's a compound, it's not NIL.
			CTerm keyTerm = ((CCompound) tree).getArgument(1);
			String keyString = keyTerm.getFunctorValue();
			int c = key.compareTo(keyString);
			if (c < 0) {
				tree = ((CCompound) tree).getArgument(0);
			} else if (c == 0) {
				return tree = ((CCompound) tree).getArgument(2);
			} else if (c > 0) {
				tree = ((CCompound) tree).getArgument(3);
			}
		}
		// in a correctly formed rbtree, the invariant only fails if tree is
		// NIL.
		return null;
	}

	private static class _rbTreeNodeIterator implements Iterator {

		/*
		 * invariance: the left-most node that was not yet returned is top on
		 * stack.
		 */

		private LinkedList<CTerm> stack = new LinkedList<CTerm>();

		public _rbTreeNodeIterator(CTerm root) {
			diveLeft(root);
		}

		public boolean hasNext() {
			return !stack.isEmpty();
		}

		public Object next() {
			// climb up
			CCompound top = (CCompound) stack.removeLast();
			// left subtree is already done. ->dive right
			if (hasRightChild(top)) {
				CCompound right = (CCompound) top.getArgument(3);
				diveLeft(right);
			}

			return top;
		}

		private void diveLeft(CTerm node) {
			while (!isEmptyTree(node)) {
				stack.addLast(node);
				node = ((CCompound) node).getArgument(0);

			}

		}

		private boolean isEmptyTree(CTerm c) {
			return isEmptyTree((CCompound) c);
		}

		private boolean isEmptyTree(CCompound c) {
			return "black".equals(c.getFunctorValue())
					&& c.getArgument(0) instanceof CNil
					&& c.getArgument(1) instanceof CNil
					&& c.getArgument(2) instanceof CNil
					&& c.getArgument(3) instanceof CNil;
		}

		private boolean hasLeftChild(CTerm node) {
			if (!(node instanceof CCompound)) {
				return false;
			}
			CCompound c = (CCompound) node;

			return !isEmptyTree((CCompound) c.getArgument(0));
		}

		private boolean hasRightChild(CTerm node) {
			if (!(node instanceof CCompound)) {
				return false;
			}
			CCompound c = (CCompound) node;

			return !isEmptyTree((CCompound) c.getArgument(3));
		}

		public void remove() {
			throw new UnsupportedOperationException();

		}

	}

	public static Iterator rbtreeIterateNodes(CTerm tree) {
		return new _rbTreeNodeIterator(tree);

	}

	

	
	public static CTerm createCTerm(Object input) {
		ATermFactory factory = new ATermFactory();
		return factory.createCTerm(input);
	}

	public static String renderTerm(CTerm term) {
		StringBuffer sb = new StringBuffer();
		renderTerm(term, sb);
		return sb.toString();
	}

	private static void renderTerm(CTerm term, StringBuffer sb) {

		if (term instanceof CVariable) {
			sb.append(((CVariable) term).getVariableName());
		} else {
			sb.append(term.getFunctorImage());
		}
		if (term.getArity() > 0) {
			sb.append('(');
			CCompound compound = (CCompound) term;
			for (int i = 0; i < compound.getArity(); i++) {
				if (i > 0) {
					sb.append(", ");
				}
				renderTerm(compound.getArgument(i), sb);
			}
			sb.append(')');
		}

	}

	public static String renderSignature(CTerm sig, String defaultModule) {
		CCompound term = (CCompound) sig;
		String module = defaultModule;
		if (":".equals(term.getFunctorValue())) {
			module = term.getArgument(0).getFunctorValue();
			term = (CCompound) term.getArgument(1);
		}
		String name = term.getArgument(0).getFunctorValue();
		int arity = ((CInteger) term.getArgument(1)).getIntValue();
		return module + ":" + name + "/" + arity;
	}
}
