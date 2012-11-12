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

package org.cs3.pdt.common.search;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;

import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.queries.PDTSearchQuery;
import org.cs3.pdt.common.structureElements.ModuleMatch;
import org.cs3.pdt.common.structureElements.PredicateMatch;
import org.cs3.pdt.common.structureElements.PrologMatch;
import org.cs3.pdt.common.structureElements.SearchFileTreeElement;
import org.cs3.pdt.common.structureElements.SearchMatchElement;
import org.cs3.pdt.common.structureElements.SearchModuleElement;
import org.cs3.pdt.common.structureElements.SearchPredicateElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.IEditorMatchAdapter;
import org.eclipse.search.ui.text.IFileMatchAdapter;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @author rho
 * 
 */
public class PrologSearchResult extends AbstractTextSearchResult implements
		IEditorMatchAdapter, IFileMatchAdapter {

	private PDTSearchQuery query;
	private Goal goal;
	private final Match[] EMPTY_ARR = new Match[0];

	/**
	 * @param query
	 * @param queryString
	 */
	public PrologSearchResult(PDTSearchQuery query, Goal goal) {
		this.query = query;
		this.goal = goal;
	}

	@Override
	public IEditorMatchAdapter getEditorMatchAdapter() {
		return this;
	}

	@Override
	public IFileMatchAdapter getFileMatchAdapter() {
		return this;
	}

	private String searchType = "Prolog Search";
	private LinkedHashMap<String, SearchModuleElement> modules = new LinkedHashMap<String, SearchModuleElement>();
	@Override
	public final String getLabel() {		
		return searchType + " "
		       + (goal==null ? "oops, goal is null?!" : goal.getTermString()/*goal.getModule()+":"+goal.getFunctor()+"/"+goal.getArity()*/ )
		       + " \u2013 "
		       + getMatchCount() 
		       + " matches in active Prolog Console";
	}
	public void setSearchType(String s) {
		searchType=s;
	}

	@Override
	public String getTooltip() {
		return getLabel();
	}

	@Override
	public ImageDescriptor getImageDescriptor() {
		// PrologElementAdaptable element = new PrologElementAdaptable(data);
		// return
		// ((IWorkbenchAdapter)element.getAdapter(IWorkbenchAdapter.class)).getImageDescriptor(null);
		return null;
	}

	@Override
	public ISearchQuery getQuery() {
		return query;
	}

	@Override
	public boolean isShownInEditor(Match match, IEditorPart editor) {
		IEditorInput ei = editor.getEditorInput();
		if (ei instanceof IFileEditorInput && match instanceof PrologMatch) {
			FileEditorInput fi = (FileEditorInput) ei;
			IFile fileForMatch = ((PrologMatch)match).getFile();
			return (fileForMatch != null && fileForMatch.equals(fi.getFile()));
		}
		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.search.ui.text.IEditorMatchAdapter#computeContainedMatches(org.eclipse.search.ui.text.AbstractTextSearchResult,
	 *      org.eclipse.ui.IEditorPart)
	 */
	@Override
	public Match[] computeContainedMatches(AbstractTextSearchResult result,
			IEditorPart editor) {
		IEditorInput ei = editor.getEditorInput();
		if (ei instanceof IFileEditorInput) {
			FileEditorInput fi = (FileEditorInput) ei;
			return computeContainedMatches(fi.getFile());
		}
		else return EMPTY_ARR;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.search.ui.text.IFileMatchAdapter#computeContainedMatches(org.eclipse.search.ui.text.AbstractTextSearchResult,
	 *      org.eclipse.core.resources.IFile)
	 */
	@Override
	public Match[] computeContainedMatches(AbstractTextSearchResult result,
			IFile file) {
		return computeContainedMatches(file);
	}
	
	private Match[] computeContainedMatches(IFile file) {
		HashSet<Match> result = new HashSet<Match>();
		for (SearchModuleElement module : modules.values()) {
			for (Object obj : module.getChildren()) {
				if (obj instanceof SearchPredicateElement) {
					SearchPredicateElement predicate = (SearchPredicateElement) obj;
					for (SearchFileTreeElement fileTreeElement : predicate.getFileTreeElements()) {
						IFile fileForTreeElement = fileTreeElement.getFile();
						if (fileForTreeElement != null && fileForTreeElement.equals(file)) {
							for (PrologMatch match : fileTreeElement.getOccurrences()) {
								result.add(match);
							}
						}
					}
				}
			}
		}
		return result.toArray(new Match[result.size()]);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.search.ui.text.IFileMatchAdapter#getFile(java.lang.Object)
	 */
	@Override
	public IFile getFile(Object element) {
		if (element instanceof IFile){
			return (IFile) element;
		} else if (element instanceof SearchFileTreeElement) {
			return ((SearchFileTreeElement) element).getFile();
		} else if (element instanceof SearchMatchElement){
			return ((SearchMatchElement) element).getMatch().getFile();
		} else if (element instanceof PrologMatch) {
			return ((PrologMatch) element).getFile();
		} else {
			return null;
		}
	}
	
	public SearchModuleElement[] getModules(){
		Collection<SearchModuleElement> values = modules.values();
		SearchModuleElement[] moduleArray = values.toArray(new SearchModuleElement[values.size()]);
		Arrays.sort(moduleArray);
		return moduleArray;
	}
	
	public Object[] getChildren() {
		return getModules();
	}
	
	
	@Override
	public void addMatch(Match match) {
		super.addMatch(match);
		addMatchToResult(match);
	}
	
	@Override
	public void addMatches(Match[] matches) {
		super.addMatches(matches);
		for (Match match : matches) {
			addMatchToResult(match);
		}
	}
	
	private void addMatchToResult(Match match) {
		if (match instanceof PrologMatch) {
			PrologMatch prologMatch = (PrologMatch) match;
			String module = prologMatch.getModule();
			String visibility = prologMatch.getVisibility();
			String signature = module + visibility;
			SearchModuleElement searchModuleElement = modules.get(signature);
			if (searchModuleElement == null) {
				searchModuleElement = new SearchModuleElement(this, module, visibility);
				modules.put(signature, searchModuleElement);
			}
			searchModuleElement.addMatch(prologMatch);
		} else if (match instanceof PredicateMatch) {
			PredicateMatch predicateMatch = (PredicateMatch) match;
			String module = predicateMatch.getModule();
			String visibility = predicateMatch.getVisibility();
			String signature = module + visibility;
			SearchModuleElement searchModuleElement = modules.get(signature);
			if (searchModuleElement == null) {
				searchModuleElement = new SearchModuleElement(this, module, visibility);
				modules.put(signature, searchModuleElement);
			}
			searchModuleElement.addMatch(predicateMatch);
		} else if (match instanceof ModuleMatch) {
			ModuleMatch moduleMatch = (ModuleMatch) match;
			SearchModuleElement element = (SearchModuleElement) moduleMatch.getElement();
			element.setParent(this);
			modules.put(moduleMatch.getModule(), element);
		}
	}
	
	@Override
	public void removeAll() {	
		super.removeAll();
		modules.clear();
	}
	
	@Override
	public void removeMatch(Match match) {
		super.removeMatch(match);
		removeMatchFromResult((PrologMatch) match);
	}
	
	@Override
	public void removeMatches(Match[] matches) {
		super.removeMatches(matches);
		for (Match match : matches) {
			removeMatchFromResult(match);
		}
	}
	
	private void removeMatchFromResult(Match match) {
		if (match instanceof PrologMatch) {
			PrologMatch prologMatch = (PrologMatch) match;
			String signature = prologMatch.getModule() + prologMatch.getVisibility();
			SearchModuleElement searchModuleElement = modules.get(signature);
			if (searchModuleElement != null) {
				searchModuleElement.removeMatch(prologMatch);
				if (!searchModuleElement.hasChildren()) {
					modules.remove(signature);
				}
			}
		} else if (match instanceof PredicateMatch) {
			PrologMatch predicateMatch = (PrologMatch) match;
			String signature = predicateMatch.getModule() + predicateMatch.getVisibility();
			SearchModuleElement searchModuleElement = modules.get(signature);
			if (searchModuleElement != null) {
				searchModuleElement.removeMatch(predicateMatch);
				if (!searchModuleElement.hasChildren()) {
					modules.remove(signature);
				}
			}
		} else if (match instanceof ModuleMatch) {
			ModuleMatch moduleMatch = (ModuleMatch) match;
			modules.remove(moduleMatch.getModule());
		}
	}

}


