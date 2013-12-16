/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.common.structureElements;

import java.util.ArrayList;
import java.util.LinkedHashMap;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;

public class SearchFileTreeElement implements PrologSearchTreeElement, IAdaptable {
	
	private LinkedHashMap<String, SearchMatchElement> matchesToSearchElements = new LinkedHashMap<String, SearchMatchElement>();
	private IFile file;
	private Object parent;
	
	public SearchFileTreeElement(Object parent, IFile file) {
		this.file = file;
		this.parent = parent;
	}
	
	public IFile getFile() {
		return file;
	}
	
	@Override
	public boolean hasChildren() {
		return !(matchesToSearchElements.isEmpty());
	}

	@Override
	public Object[] getChildren() {
		return matchesToSearchElements.values().toArray();
	}
	
	public PrologMatch getFirstMatch() {
		if (matchesToSearchElements.isEmpty()) {
			return null;
		}
		PrologMatch firstMatch = null;
		int firstLine = Integer.MAX_VALUE;
		for (SearchMatchElement element : matchesToSearchElements.values()) {
			PrologMatch occurence = element.getMatch();
			int line = occurence.getLine();
			if (firstMatch == null) {
				firstMatch = occurence;
				firstLine = line;
			} else if (line < firstLine) {
				firstLine = line;
				firstMatch = occurence;
			}
		}
		return firstMatch;
	}

	@Override
	public String getLabel() {
		return file.getName();
	}

	public void removeMatch(PrologMatch match) {
		String signature = match.getSignature();
		SearchMatchElement searchMatchElement = matchesToSearchElements.get(signature);
		if (searchMatchElement != null) {
			searchMatchElement.removeMatch(match);
			if (searchMatchElement.computeContainedMatches() <= 0) {
				matchesToSearchElements.remove(signature);
			}
		}
	}

	public void addMatch(PrologMatch match) {
		SearchMatchElement searchMatchElement = matchesToSearchElements.get(match.getSignature());
		if (searchMatchElement == null) {
			searchMatchElement = (SearchMatchElement) match.getElement();
			searchMatchElement.setParent(this);
			matchesToSearchElements.put(match.getSignature(), searchMatchElement);
		}
		searchMatchElement.addMatch(match);
	}

	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public Object getAdapter(@SuppressWarnings("rawtypes") Class adapter) {
		if (adapter.equals(IResource.class)) {
			return file;
		} else { 
			return null;
		}
	}

	@Override
	public int computeContainedMatches() {
		int count = 0;
		for (SearchMatchElement element : matchesToSearchElements.values()) {
			count += element.computeContainedMatches();
		}
		return count;
	}

	@Override
	public void collectContainedMatches(IFile file, ArrayList<PrologMatch> matches) {
		if (file.equals(this.file)) {
			for (SearchMatchElement element : matchesToSearchElements.values()) {
				element.collectContainedMatches(file, matches);
			}
		}
	}

}


