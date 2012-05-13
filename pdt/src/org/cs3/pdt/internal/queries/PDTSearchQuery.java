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

package org.cs3.pdt.internal.queries;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDT;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.search.PrologSearchResult;
import org.cs3.pdt.internal.structureElements.PDTMatch;
import org.cs3.pdt.internal.structureElements.SearchMatchElement;
import org.cs3.pdt.internal.structureElements.SearchModuleElement;
import org.cs3.pdt.internal.structureElements.SearchPredicateElement;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;

public abstract class PDTSearchQuery implements ISearchQuery {

	private Goal goal;
	private PrologSearchResult result;
	private Map<String, SearchPredicateElement> predForSignature = new HashMap<String, SearchPredicateElement>();
	private Map<String, SearchModuleElement> moduleElements = new HashMap<String, SearchModuleElement>();
	private LinkedHashSet<String> signatures = new LinkedHashSet<String>();

	public PDTSearchQuery(PrologInterface pif, Goal goal) {
		this.goal = goal;
		result = new PrologSearchResult(this, goal);
	}
    
	// Adapt the text in the header of the search result view:
	protected void setSearchType(String s)  {
		result.setSearchType(s);
	}
	
	@Override
	public IStatus run(IProgressMonitor monitor) {
		try {
			return run_impl(monitor);
		} catch (Throwable t) {
			Debug.report(t);
			return new Status(IStatus.ERROR,PDT.PLUGIN_ID,42,"Exception caught during search.",t);
		}
	}
	
	// TODO: The progress monitor parameter is never used. 
	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException, PrologException, PrologInterfaceException {
		result.removeAll();
		if(goal==null){
			Debug.error("Search goal data is null!");
			throw new NullPointerException();
		}

//		if(pif!= null){ // The project has the PDT nature
//			return searchIfPDTNatureIsSet();
//		} else {                                 
			return searchIfPDTNatureIsMissing(); 
//		}
	}

	/**
	 * @return
	 * @throws PrologInterfaceException
	 * @throws PrologException
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private IStatus searchIfPDTNatureIsMissing()
			throws PrologInterfaceException, PrologException, IOException,
			NumberFormatException {

		if(PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole() == null){
			return Status.CANCEL_STATUS;
		}
		else {
			PrologSession session = PrologConsolePlugin.getDefault().getPrologConsoleService()
			                        .getActivePrologConsole().getPrologInterface().getSession();

			processFoundClauses(findReferencedClauses(session));
			return Status.OK_STATUS;
		}
	}

	/**
	 * @param session
	 * @return
	 * @throws PrologException
	 * @throws PrologInterfaceException
	 */
	private List<Map<String, Object>> findReferencedClauses(PrologSession session)
			throws PrologException, PrologInterfaceException {
		
		String module = "Module";               // Modul ist freie Variable
		if(goal.getModule()!=null)
			module ="'"+ goal.getModule()+ "'"; // Modul ist explizit gesetzt

		String query = buildSearchQuery(goal, module);
		
		predForSignature.clear();
		
		List<Map<String, Object>> clauses = getResultForQuery(session, query);
		
		// Bindung der Modulvariablen aus vorheriger Query abfragen und im Goal setzen.
		if(clauses.size()>0 && goal.getModule()==null){
			goal.setModule(clauses.get(0).get("Module").toString());
		}
		return clauses;
	}
	
	abstract protected String buildSearchQuery(Goal goal, String module);

	protected List<Map<String, Object>> getResultForQuery(PrologSession session, String query) 
			throws PrologInterfaceException {
		Debug.info(query);
				
		List<Map<String, Object>> clauses = session.queryAll(query);
        return clauses;
	}

	/**
	 * @param clauses
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private void processFoundClauses(List<Map<String, Object>> clauses)
	throws IOException, NumberFormatException {
		PDTMatch match;
		moduleElements.clear();
		predForSignature.clear();
		signatures.clear();
		for (Iterator<Map<String,Object>> iterator = clauses.iterator(); iterator.hasNext();) {
			Map<String,Object> m = iterator.next();
			Debug.info(m.toString());
			match = constructPrologMatchForAResult(m);
			if ((result != null) && (match != null)) {
				result.addMatch(match);
			}
		}
	}
	
	protected abstract PDTMatch constructPrologMatchForAResult(Map<String,Object> m) throws IOException;

	protected PDTMatch createUniqueMatch(String definingModule, String functor, int arity, IFile file, int line, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + line;
		if (signatures.contains(signature)) {
			return null;
		} else {
			signatures.add(signature);
			SearchMatchElement searchMatchElement = new SearchMatchElement();
			PDTMatch match = new PDTMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, line, declOrDef);
			searchMatchElement.setMatch(match);
			return match;
		}
	}

	protected PDTMatch createUniqueMatch(String definingModule, String functor, int arity, IFile file, int offset, int length, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + offset + "#" + length;
		if (signatures.contains(signature)) {
			return null;
		} else {
			signatures.add(signature);
			SearchMatchElement searchMatchElement = new SearchMatchElement();
			PDTMatch match = new PDTMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, offset, length, declOrDef);
			searchMatchElement.setMatch(match);
			return match;
		}
	}

	@Override
	public String getLabel() {
		return "Prolog Query: " + goal.getSignature();
	}

	@Override
	public boolean canRerun() {
		return true;
	}

	@Override
	public boolean canRunInBackground() {
		return false;
	}

	@Override
	public ISearchResult getSearchResult() {
		return result;
	}

	protected void setGoal(Goal goal) {
		this.goal = goal;
	}

	protected Goal getGoal() {
		return goal;
	}
	
}