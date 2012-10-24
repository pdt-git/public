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
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Vector;

import org.cs3.pdt.common.metadata.Predicate;
import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a predicate.
 */
public class SearchPredicateElement extends Predicate implements PrologSearchTreeElement {

	private static final long serialVersionUID = 8822257720982382862L;
	
	private LinkedHashMap<IFile, SearchFileTreeElement> fileToFileTreeElement = new LinkedHashMap<IFile, SearchFileTreeElement>();
	private Object parent;

	private ArrayList<PredicateMatch> matches;
	
	public SearchPredicateElement(Object parent, String module, String predicateName, int arity, List<String> properties) {
		super(module,predicateName,arity, properties);
		this.parent = parent;
	}
	
	public SearchPredicateElement(Object parent, String module, String predicateName, int arity) {
		super(module, predicateName, arity, new Vector<String>());
		this.parent = parent;
	}
	
	@Override
	public boolean hasChildren() {
		return !fileToFileTreeElement.values().isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return fileToFileTreeElement.values().toArray();
	}
	
	@Override
	public String getLabel() {
		return getFunctor() + "/" + getArity();
	}
	
	public int numberOfOccurences() {
		if (matches == null) {
			int sum = 0;
			for (SearchFileTreeElement e : fileToFileTreeElement.values()) {
				sum += e.getNumberOfChildren();
			}
			return sum;
		} else {
			return matches.size();
		}
	}
	
	public PrologMatch getFirstOccurrence() {
		if (fileToFileTreeElement.values().isEmpty()) {
			return null;
		} else {
			for (SearchFileTreeElement e : fileToFileTreeElement.values()) {
				return e.getFirstMatch();
			}
			return null;
		}
	}
	
	public PrologMatch[] getOccurrences() {
		HashSet<PrologMatch> matches = new HashSet<PrologMatch>();
		for (SearchFileTreeElement e : fileToFileTreeElement.values()) {
			for (PrologMatch m : e.getOccurrences()) {
				matches.add(m);
			}
		}
		return matches.toArray(new PrologMatch[matches.size()]);
	}
	
	public SearchFileTreeElement[] getFileTreeElements() {
		Collection<SearchFileTreeElement> values = fileToFileTreeElement.values();
		return values.toArray(new SearchFileTreeElement[values.size()]);
	}

	@Override
	public void removeMatch(PrologMatch match) {
		IFile file = match.getFile();
		if (fileToFileTreeElement.containsKey(file)) {
			SearchFileTreeElement fileTreeElement = fileToFileTreeElement.get(file);
			fileTreeElement.removeMatch(match);
			if (!fileTreeElement.hasChildren()) {
				fileToFileTreeElement.remove(file);
			}
		}
	}

	@Override
	public void addMatch(PrologMatch match) {
		SearchFileTreeElement fileTreeElement = fileToFileTreeElement.get(match.getFile());
		if (fileTreeElement == null) {
			fileTreeElement = new SearchFileTreeElement(this, match.getFile());
			fileToFileTreeElement.put(fileTreeElement.getFile(), fileTreeElement);
		}
		fileTreeElement.addMatch(match);
	}

	@Override
	public Object getParent() {
		return parent;
	}

	@Override
	public boolean equals(Object object) {
		return this == object;
	}

	public void addMatch(PredicateMatch match) {
		if (matches == null) {
			matches = new ArrayList<PredicateMatch>();
		}
		matches.add(match);
	}
	
	public void removeMatch(PredicateMatch match) {
		if (matches != null) {
			matches.remove(match);
		}
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}

	public boolean hasMatches() {
		return (matches != null && !matches.isEmpty());
	}
	
}


