/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.cs3.pdt.internal.search;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.search.ui.text.Match;


public class PrologSearchTreeContentProvider extends PrologSearchContentProvider implements ITreeContentProvider {
	PrologSearchTreeContentProvider(PrologSearchResultPage page) {
		super(page);
	}

	@Override
	protected synchronized void initialize(PrologSearchResult result) {
		super.initialize(result);
	}

	@Override
	public Object getParent(Object child) {
		if(child==null||getSearchResult()==null){
			return null;
		}
		if(child instanceof IFile){
			return getSearchResult();			//TODO: fix this
		}
		if(child instanceof PredicateElement ){
			return getSearchResult().getFile(child);
		}
		if(child instanceof Match){
			Match match = (Match) child;
			return match.getElement();
		} if (child instanceof SearchResultCategory) {
			return getSearchResult();
		}
		return null;
	}

	

	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement==null||getSearchResult()==null){
			return null;
		}
		if (parentElement instanceof PrologSearchResult){
			return getSearchResult().getChildren();
		}
		if (parentElement instanceof SearchResultCategory){
			List<PrologMatch> matches = ((SearchResultCategory) parentElement).getMatches();
			return ModuleDummyCreator.getModuleDummiesForMatches(matches);
		}
		if (parentElement instanceof ModuleSearchDummy) {
			return ((ModuleSearchDummy) parentElement).getFiles();
		}
		if (parentElement instanceof IFile){
			return getSearchResult().getElements((IFile) parentElement);
		}
		if (parentElement instanceof PredicateElement){
			return getSearchResult().getMatches(parentElement);	
		}
		return null;
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element==null||getSearchResult()==null){
			return false;
		}
		return element instanceof IFile || 
				element instanceof PrologSearchResult || 
				element instanceof PredicateElement ||
				element instanceof SearchResultCategory ||
				element instanceof ModuleSearchDummy;
	}

	@Override
	public void clear() {
		initialize(getSearchResult());
		getPage().getViewer().refresh();
	}

	@Override
	public void elementsChanged(Object[] updatedElements) {
		clear();
	}
}
