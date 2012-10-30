/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.queries;

import java.io.IOException;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;

import javax.swing.text.BadLocationException;

import org.cs3.pdt.common.PDTCommonPlugin;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.search.PrologSearchResult;
import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.common.structureElements.PredicateMatch;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.common.structureElements.SearchMatchElement;
import org.cs3.pdt.common.structureElements.SearchPredicateElement;
import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologException;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.ISearchResult;
import org.eclipse.search.ui.text.Match;

public abstract class PDTSearchQuery implements ISearchQuery {

	private Goal goal;
	private PrologSearchResult result;
	private LinkedHashSet<String> signatures = new LinkedHashSet<String>();
	private LinkedHashMap<String, SearchPredicateElement> predicates = new LinkedHashMap<String, SearchPredicateElement>();

	public PDTSearchQuery(Goal goal) {
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
			return new Status(IStatus.ERROR, PDTCommonPlugin.PLUGIN_ID, 42, "Exception caught during search.", t);
		}
	}
	
	private IStatus run_impl(IProgressMonitor monitor) throws CoreException,
			BadLocationException, IOException, PrologException, PrologInterfaceException {
		result.removeAll();
		if(goal==null){
			Debug.error("Search goal data is null!");
			throw new NullPointerException();
		}

		return doSearch(monitor); 
	}

	/**
	 * @return
	 * @throws PrologInterfaceException
	 * @throws PrologException
	 * @throws IOException
	 * @throws NumberFormatException
	 */
	private IStatus doSearch(IProgressMonitor monitor) throws PrologInterfaceException, PrologException, IOException, NumberFormatException {
		PrologSession session = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface().getSession();
		monitor.beginTask("Searching...", 2);
		monitor.subTask("Running Prolog query");
		List<Map<String, Object>> results = findReferencedClauses(session, new SubProgressMonitor(monitor, 1));
		monitor.subTask("Processing results");
		processFoundClauses(results, new SubProgressMonitor(monitor, 1));
		monitor.done();
		return Status.OK_STATUS;
	}

	/**
	 * @param session
	 * @return
	 * @throws PrologException
	 * @throws PrologInterfaceException
	 */
	private List<Map<String, Object>> findReferencedClauses(PrologSession session, IProgressMonitor monitor)
			throws PrologException, PrologInterfaceException {
		
		monitor.beginTask("", 1);
		String module;               
		if (goal.getModule() != null && !goal.getModule().isEmpty()) {
			module = Util.quoteAtomIfNeeded(goal.getModule());
		} else {
			module = "Module";                  // Modul is free variable
		}

		String query = buildSearchQuery(goal, module);
		
		List<Map<String, Object>> clauses = getResultForQuery(session, query);
		
		// Bindung der Modulvariablen aus vorheriger Query abfragen und im Goal setzen.
//		if (clauses.size() > 0 && goal.getModule() == null){
//			goal.setModule(clauses.get(0).get("Module").toString());
//		}
		monitor.done();
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
	private void processFoundClauses(List<Map<String, Object>> clauses, IProgressMonitor monitor)
	throws IOException, NumberFormatException {
		Match match;
		signatures.clear();
		predicates.clear();
		monitor.beginTask("", clauses.size());
		for (Iterator<Map<String,Object>> iterator = clauses.iterator(); iterator.hasNext();) {
			Map<String,Object> m = iterator.next();
			Debug.info(m.toString());
			match = constructPrologMatchForAResult(m);
			if ((result != null) && (match != null)) {
				result.addMatch(match);
			}
			monitor.worked(1);
		}
		monitor.done();
	}
	
	protected abstract Match constructPrologMatchForAResult(Map<String,Object> m) throws IOException;

	protected PrologMatch createUniqueMatch(String definingModule, String functor, int arity, IFile file, int line, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + line;
		if (signatures.contains(signature)) {
			return null;
		} else {
			signatures.add(signature);
			SearchMatchElement searchMatchElement = new SearchMatchElement();
			PrologMatch match = new PrologMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, line, declOrDef);
			searchMatchElement.setMatch(match);
			return match;
		}
	}

	protected PrologMatch createUniqueMatch(String definingModule, String functor, int arity, IFile file, int offset, int length, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + offset + "#" + length;
		if (signatures.contains(signature)) {
			return null;
		} else {
			signatures.add(signature);
			SearchMatchElement searchMatchElement = new SearchMatchElement();
			PrologMatch match = new PrologMatch(searchMatchElement, visibility, definingModule, functor, arity, properties, file, offset, length, declOrDef);
			searchMatchElement.setMatch(match);
			return match;
		}
	}
	
	protected PredicateMatch createUniqueMatch(String definingModule, String functor, int arity, List<String> properties, String visibility, String declOrDef) {
		String signature = declOrDef + visibility + definingModule + functor + arity + "#";
		if (signatures.contains(signature)) {
			return null;
		} else {
			signatures.add(signature);
			String predicateSignature = visibility + definingModule + functor + arity;
			SearchPredicateElement searchPredicateElement = predicates.get(predicateSignature);
			if (searchPredicateElement == null) {
				searchPredicateElement = new SearchPredicateElement(null, definingModule, functor, arity, properties);
				predicates.put(predicateSignature, searchPredicateElement);
			}
			PredicateMatch match = new PredicateMatch(searchPredicateElement, visibility, definingModule, functor, arity, properties, declOrDef);
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
	
	protected IFile findFile(String fileName) throws IOException {
		if (fileName == null || SearchConstants.RESULT_KIND_DYNAMIC.equals(fileName) || SearchConstants.RESULT_KIND_FOREIGN.equals(fileName)) {
			return null;
		} else {
			return FileUtils.findFileForLocation(fileName);
		}
	}
	
}


