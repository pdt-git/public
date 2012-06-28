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

package org.cs3.pdt.internal.search;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

import org.cs3.pdt.internal.queries.PDTSearchQuery;
import org.cs3.pdt.internal.structureElements.SearchFileTreeElement;
import org.cs3.pdt.internal.structureElements.PrologMatch;
import org.cs3.pdt.internal.structureElements.SearchMatchElement;
import org.cs3.pdt.internal.structureElements.SearchModuleElement;
import org.cs3.pdt.internal.structureElements.SearchPredicateElement;
import org.cs3.pl.metadata.Goal;
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
	private HashMap<String, SearchModuleElement> modules = new HashMap<String, SearchModuleElement>();
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
			return ((PrologMatch)match).getFile().equals(fi.getFile());
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
						if (fileTreeElement.getFile().equals(file)) {
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
		addMatchToResult((PrologMatch) match);
	}
	
	@Override
	public void addMatches(Match[] matches) {
		super.addMatches(matches);
		for (Match match : matches) {
			addMatchToResult((PrologMatch) match);
		}
	}
	
	private void addMatchToResult(PrologMatch match) {
		String module = match.getModule();
		String visibility = match.getVisibility();
		String signature = module + visibility;
		SearchModuleElement searchModuleElement = modules.get(signature);
		if (searchModuleElement == null) {
			searchModuleElement = new SearchModuleElement(this, module, visibility);
			modules.put(signature, searchModuleElement);
		}
		searchModuleElement.addMatch(match);
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
			removeMatchFromResult((PrologMatch) match);
		}
	}
	
	private void removeMatchFromResult(PrologMatch match) {
		String signature = match.getModule() + match.getVisibility();
		SearchModuleElement searchModuleElement = modules.get(signature);
		if (searchModuleElement != null) {
			searchModuleElement.removeMatch(match);
			if (!searchModuleElement.hasChildren()) {
				modules.remove(signature);
			}
		}
	}

}
