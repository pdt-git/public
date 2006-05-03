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

package org.cs3.pl.metadata.internal.classic;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.ClauseData;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.PredicateData;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;

/**
 * This class is intended as a TEMPORARY solution. it contains query related
 * conveniance methods, formerly found in the PrologClient.
 */
public class DefaultMetaInfoProvider implements IMetaInfoProvider {
	public static final boolean windowsPlattform = System
			.getProperty("os.name").indexOf("Windows") > -1;

	private PrologInterface pif = null;

	private String pdtModulePrefix = "";

	public DefaultMetaInfoProvider(PrologInterface pif) {
		this.pif = pif;
	}

	public DefaultMetaInfoProvider(PrologInterface pif, String prefix) {
		this.pif = pif;
		this.pdtModulePrefix = prefix;
	}

	
	
	

	/**
	 * @param file
	 * @return
	 * @throws PrologException
	 * @throws NumberFormatException
	 * @throws PrologInterfaceException 
	 */

	public Predicate[] getPredicatesWithPrefix(String module, String prefix)
			throws NumberFormatException, PrologException, PrologInterfaceException {
		return getPredicatesWithPrefix(module, prefix, null);
	}

	/**
	 * //TODO: add pos, len(?), dyn, multi.. Retrieves Predicates with prefix
	 * <i>prefix </i>. There to ways to restrict the returned elements: module
	 * and filename
	 * 
	 * @param prefix
	 * @param module
	 *            can be null -> no restriction on the module
	 * @param filename
	 *            can be null -> no restriction on the declaring file
	 * @module Module name or null, if module is not defined.
	 * @return
	 * @throws PrologException
	 * @throws NumberFormatException
	 * @throws PrologInterfaceException 
	 */
	public Predicate[] getPredicatesWithPrefix(String module, String prefix,
			String filename) throws NumberFormatException, PrologException, PrologInterfaceException {
		// return
		// (PrologElementData[])predicates.get(makeFilenameSWIConform(filename));
		PrologSession session = pif.getSession();

		if (module == null)
			module = "_";
		if (filename == null)
			filename = "_";
		String query = pdtModulePrefix + "find_pred('"
				+ filename + "','" + prefix + "', " + module
				+ ",Name,Arity,Public)";
		List results = session.queryAll(query);
		List list = new ArrayList();
		// while (result != null) {
		for (Iterator it = results.iterator(); it.hasNext();) {
			Map result = (Map) it.next();
			boolean pub = Boolean.valueOf(result.get("Public").toString())
					.booleanValue();
			Predicate data = new PredicateData(module, result.get("Name")
					.toString(), Integer.parseInt(result.get("Arity")
					.toString()), pub, false, false);
			list.add(data);

		}
		session.dispose();
		return (Predicate[]) list.toArray(new Predicate[0]);
	}

	public String getSummary(Predicate data) throws PrologException, PrologInterfaceException {
		String help = getHelp(data);
		if (help == null)
			return null;
		return help.substring(0, help.indexOf('\n'));
	}

	public Clause[] retrievePrologElements(String file) throws PrologException, PrologInterfaceException {
		PrologSession session = pif.getSession();

		List results = session.queryAll(/*
										 * "bagof([Pos_,Len_]," +
										 */"meta_data" + "('" + file
				+ "',Module,Name,Arity,Public,Pos,Len, Dyn,Mul)"
		/* +",[[Pos,Len]|_])" */);
		List list = new ArrayList();
		for (Iterator it = results.iterator(); it.hasNext();) {
			Map result = (Map) it.next();
			// debug(result.get("Name").toString()+" - PUBLIC-
			// "+Boolean.valueOf(result.get("Public").toString()).booleanValue());

			SourceLocation sl = new SourceLocation(file, true, false);
			sl.offset = Integer.parseInt(result.get("Pos").toString());
			sl.endOffset = sl.offset
					+ Integer.parseInt(result.get("Len").toString());
			Clause data = new ClauseData(result.get("Module").toString(),
					result.get("Name").toString(), java.lang.Integer
							.parseInt(result.get("Arity").toString()), Boolean
							.valueOf(result.get("Public").toString())
							.booleanValue(), result.get("Dyn").toString()
							.equals("1"), result.get("Mul").toString().equals(
							"1"), sl);
			list.add(data);

		}
		session.dispose();
		return (Clause[]) list.toArray(new Clause[0]);
	}

	public String getHelp(Predicate data) throws PrologInterfaceException {

		PrologSession session = pif.getSession();
		Map table = null;
		try {
			table = session.queryOnce("manual_entry(" + data.getName() + ","
					+ data.getArity() + ",Info)");
		} catch (PrologException e) {
			Debug.report(e);
		} finally {
			session.dispose();
		}
		if (table != null)
			return table.get("Info").toString().replaceAll("\\\\n", "\n");
		return null;
	}

	public SourceLocation[] findReferences(Predicate data) {
		// TODO Auto-generated method stub
		return null;
	}

	public Clause[] findClauses(Predicate p) throws PrologInterfaceException {
		PrologSession session = pif.getSession();
		try {
			
			String query= "meta_data(File,"
				+"'"+p.getModule()+"',"
				+"'"+p.getName()+"',"
				+p.getArity()+","
				+"_,"//wether it is public - we already know this.
				+"Pos,"
				+"Len,"
				+"_,"//wether it is dynamic - we already know this.
				+"_)"//wether it is multifile - we already know this.
				;
				
				
			List l = session.queryAll(query);
			if (l==null){
				return new Clause[0];
			}
			Clause[] result = new Clause[l.size()];
			int i=0;
			for (Iterator it = l.iterator(); it.hasNext();i++) {
				Map m = (Map) it.next();
				SourceLocation sl = new SourceLocation((String) m.get("File"),true,false);				
				sl.offset=Integer.parseInt( (String) m.get("Pos"));
				sl.endOffset=sl.offset+Integer.parseInt( (String) m.get("Len"));				
				result[i]=new ClauseData(p.getModule(),p.getName(),p.getArity(),p.isPublic(),p.isDynamic(),p.isMultifile(),sl);
			}
			return result;
		} finally {
			if (session != null) {
				session.dispose();
			}
		}

	}

	/*
	 * FIXME: the current implementation will simply guess matching predicates
	 * looking at name and arity. That is, if the correct predicate exists and its
	 * definition was parsed, it is guaranteed to be included in the result.
	 * 
	 * Best we can do atm. :-(
	 * 
	 */
	public Predicate[] findPredicates(Goal g) throws PrologInterfaceException {
		Set result = new HashSet();
		PrologSession session = pif.getSession();
		try{
			String query= "meta_data(File,"
				+"Module,"
				+"'"+g.getName()+"',"
				+g.getArity()+","
				+"Pub,"
				+"_,"
				+"_,"
				+"Dyn,"
				+"Mult)"
				;	
			List l = session.queryAll(query);
			for (Iterator it = l.iterator(); it.hasNext();) {
				Map m = (Map) it.next();
				String module = (String) m.get("Module");
				String label = g.getName();
				int arity = g.getArity();
				boolean pub = "1".equals(m.get("Pub"));
				boolean dyn = "1".equals(m.get("Dyn"));
				boolean mult = "1".equals(m.get("Mult"));
				result.add(new PredicateData(module, label,arity,pub,dyn,mult));
			}
		}
		finally{
			if(session!=null){
				session.dispose();
			}
		}
		return (Predicate[]) result.toArray(new Predicate[result.size()]);
	}

}
