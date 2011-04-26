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
 * Created on 23.08.2004
 *
 */
package org.cs3.pdt.internal.search;

import java.util.Arrays;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Vector;

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

	private PrologSearchQuery query;
	private Goal goal;
	private final Match[] EMPTY_ARR = new Match[0];
	private HashMap<IFile,PredicateElement[]> elementCache = new HashMap<IFile, PredicateElement[]>();
	private HashSet<IFile> fileCache = new HashSet<IFile>();

	/**
	 * @param query
	 * @param queryString
	 */
	public PrologSearchResult(PrologSearchQuery query, Goal goal) {
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

	@Override
	public String getLabel() {		
		return "Prolog Search: " + (goal==null ? "oops, goal is null?!" :goal.getModule()+":"+goal.getName()+"/"+goal.getArity());
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
		if (ei instanceof IFileEditorInput) {
			FileEditorInput fi = (FileEditorInput) ei;
			return match.getElement().equals(fi.getFile());
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
			return result.getMatches(fi.getFile());
		}
		return EMPTY_ARR;
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
		return result.getMatches(file);
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
		}
		if (element instanceof PredicateElement){
			return ((PredicateElement) element).getFile();
		}
		if(element instanceof Match){
			return ((PredicateElement) ((Match) element).getElement()).getFile();
		}
		return null;
	}

	
	public Object[] getElements(IFile file) {
		PredicateElement[] children = elementCache.get(file);
		if(children==null){
			Object[] elms = getElements();
			Vector<PredicateElement> v = new Vector<PredicateElement>();
			for (int i = 0; i < elms.length; i++) {
				PredicateElement elm = (PredicateElement) elms[i];
				if(elm.getFile().equals(file)){
					v.add(elm);
				}
			}
			children = v.toArray(new PredicateElement[v.size()]);
			elementCache.put(file, children);
		}
		return children;
	}

	@Override
	public void addMatch(Match match) {
		PredicateElement elm = (PredicateElement) match.getElement();
		fileCache.add(elm.getFile());
		super.addMatch(match);
	}
	
	public IFile[] getFiles() {
		IFile[] sortedFiles = fileCache.toArray(new IFile[fileCache.size()]);
		Arrays.sort(sortedFiles,
				new Comparator<IFile>() {
					@Override
					public int compare(IFile first, IFile second) {
						return first.getFullPath().toPortableString().compareTo(second.getFullPath().toPortableString());
					}
				}
		);		
		return sortedFiles;
	}
	
	@Override
	public void removeAll() {	
		super.removeAll();
		fileCache.clear();
		elementCache.clear();
	}
}
