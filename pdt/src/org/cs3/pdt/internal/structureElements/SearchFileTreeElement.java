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

package org.cs3.pdt.internal.structureElements;

import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdaptable;

public class SearchFileTreeElement implements PrologSearchTreeElement, IAdaptable {
	
	private LinkedHashMap<PrologMatch, SearchMatchElement> matchesToSearchElements = new LinkedHashMap<PrologMatch, SearchMatchElement>();
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
	
	public PrologMatch[] getOccurrences() {
		Set<PrologMatch> keySet = matchesToSearchElements.keySet();
		return keySet.toArray(new PrologMatch[keySet.size()]);
	}
	
	public int getNumberOfChildren() {
		return matchesToSearchElements.size();
	}
	
	public PrologMatch getFirstMatch() {
		if (matchesToSearchElements.isEmpty()) {
			return null;
		}
		PrologMatch firstMatch = null;
		int firstLine = Integer.MAX_VALUE;
		for (PrologMatch occurence : matchesToSearchElements.keySet()) {
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

	@Override
	public void removeMatch(PrologMatch match) {
		SearchMatchElement removedElement = matchesToSearchElements.remove(match);
		if (removedElement != null) {
			removedElement.removeMatch(match);
		}
	}

	@Override
	public void addMatch(PrologMatch match) {
		if (!matchesToSearchElements.containsKey(match)) {
			SearchMatchElement searchMatchElement = (SearchMatchElement) match.getElement();
			searchMatchElement.setParent(this);
			matchesToSearchElements.put(match, searchMatchElement);
		}
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

}


