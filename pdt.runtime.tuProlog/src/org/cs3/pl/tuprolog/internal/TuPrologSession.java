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

/*
 */
package org.cs3.pl.tuprolog.internal;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pl.common.Option;
import org.cs3.pl.common.SimpleOption;
import org.cs3.pl.cterm.CTermFactory;
import org.cs3.pl.cterm.internal.ATermFactory;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologSession2;

import alice.tuprolog.MalformedGoalException;
import alice.tuprolog.NoMoreSolutionException;
import alice.tuprolog.NoSolutionException;
import alice.tuprolog.SolveInfo;
import alice.tuprolog.Struct;
import alice.tuprolog.Term;
import alice.tuprolog.UnknownVarException;
import alice.tuprolog.Var;

/**
 */
public class TuPrologSession implements PrologSession2 {

	private static final String OPT_CANONICAL = "socketsession.canonical";

	// socketsession.canonical

	private boolean queryActive;

	private String lastQuery;

	private TuPrologPrologInterface pif;

	private CTermFactory ctermFactory = new ATermFactory();

	private boolean disposed = false;


	public TuPrologSession(TuPrologPrologInterface pif) {
		this.pif = pif;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#dispose()
	 */
	public void dispose() {
		disposed = true;
	}

	
	private void setActiveSessionID(){
		pif.setActiveSessionID(hashCode());
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#query(java.lang.String)
	 */
	public Map query(String query) throws PrologException,
			PrologInterfaceException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		setActiveSessionID();
		SolveInfo result;
		try {
			result = pif.getEngine().solve(normalizeQuery(query));
		} catch (MalformedGoalException e) {
			throw new PrologException(e);
		}
		if(!result.isSuccess())
			return null;
		return toMap(result);
	}

	static public String normalizeQuery(String query) {
		int i = query.length()-1;
		while(query.charAt(i) == ' ' ||
				query.charAt(i) == '\n')
			i--;
		query = query.substring(0, i+1);
		if(query.charAt(query.length()-1) == '.')
			return query;
		else
			return query + "."; 
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryAll(java.lang.String)
	 */
	public List queryAll(String query) throws PrologException,
			PrologInterfaceException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		setActiveSessionID();
		synchronized (pif) {
			SolveInfo result = null;
			try {
				result = pif.getEngine().solve(normalizeQuery(query));
			} catch (MalformedGoalException e) {
				throw new PrologInterfaceException(e);
			}
			List list = new ArrayList();
			if(result == null || !result.isSuccess())
				return list;
	
			try {
				while(result.isSuccess()) {
					Map map = toMap(result);
					list.add(map);
					result = pif.getEngine().solveNext();
				}
			} catch(NoMoreSolutionException nms) {
				
			}
			return list;
			
		}

	}

	private Map toMap(SolveInfo result) throws PrologInterfaceException {
		List vars;
		Map map = new HashMap();
		try {
			vars = result.getBindingVars();
			for (Iterator iter = vars.iterator(); iter.hasNext();) {
				Var var = (Var) iter.next();
				map.put(var.getName(), 
						termToListOrString(result.getTerm(var.getName())));
			}
			/*
			for (int i = 0; i < vars.length; i++) {
				Term term = result.getTerm(vars[i].getName());
				Object value;
				map.put(vars[i].getName(),termToListOrString(term));
			}
			*/
		} catch (NoSolutionException e) {
			throw new PrologException(e);
		} catch (UnknownVarException e) {
			throw new PrologException(e);
		}
		return map;
	}

	/**
	 * Return an optionally nested list or a string 
	 * representation of the term.
	 * 
	 * @param term
	 * @return a List of a String object 
	 */
	private Object termToListOrString(Term term) {
		
		if(term.isList()) {
			List list = new ArrayList();
			Struct struct = (Struct)term;
			for (Iterator iter = struct.listIterator(); iter.hasNext();) {
				Term element = (Term) iter.next();
				list.add(termToListOrString(element));
			}
			return list;
		}else
			return term.toString();
	}
		

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#queryOnce(java.lang.String)
	 */
	public Map queryOnce(String query) throws PrologException,
			PrologInterfaceException {
		synchronized (pif) {
			Map result = query(query);
			endQuery();
			return result;
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#next()
	 */
	public Map next() throws PrologException, PrologInterfaceException {
		if (isDisposed()) {
			throw new IllegalStateException("Session is disposed!");
		}
		SolveInfo result;
		try {
			result = pif.getEngine().solveNext();
		} catch (NoMoreSolutionException e) {
			return null;
		}
		return toMap(result);

	}


	private PrologInterfaceListener dispatcher = null;

	private Option[] options;

	private boolean canonical;

	private PrologException lastError;

	/**
	 * @return Returns the dispatcher.
	 */
	public PrologInterfaceListener getDispatcher() {
		return dispatcher;
	}

	/**
	 * @param dispatcher
	 *            The dispatcher to set.
	 */
	public void setDispatcher(PrologInterfaceListener dispatcher) {
		this.dispatcher = dispatcher;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#isDisposed()
	 */
	public boolean isDisposed() {
		return disposed;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.PrologSession#getPrologInterface()
	 */
	public PrologInterface getPrologInterface() {
		return pif;
	}

	public Option[] getOptions() {
		if (options == null) {
			options = new Option[] { new SimpleOption(OPT_CANONICAL,
					"canonical values",
					"if set, the session will answer canonical terms",
					Option.FLAG, "false") };
		}
		return options;
	}

	/**
	 * this implementation does nothing.
	 */
	public void reconfigure() {
		;

	}

	public String getPreferenceValue(String id, String string) {
		if (OPT_CANONICAL.equals(id)) {
			return canonical ? "true" : "false";
		}
		throw new IllegalArgumentException("unkown option id: " + id);
	}

	public void setPreferenceValue(String id, String value) {
		if (OPT_CANONICAL.equals(id)) {
			canonical = Boolean.valueOf(value).booleanValue();
		} else {
			throw new IllegalArgumentException("unkown option id: " + id);
		}
	}

	public void endQuery() throws PrologException, PrologInterfaceException {
		pif.getEngine().solveEnd();
	}



}
