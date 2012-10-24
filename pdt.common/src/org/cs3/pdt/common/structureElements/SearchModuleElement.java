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

import java.util.LinkedHashMap;

import org.eclipse.core.resources.IFile;

public class SearchModuleElement implements PrologSearchTreeElement, Comparable<SearchModuleElement> {
	
	private String label;
	private int visibilityCode;
	private String name;
	private Object parent;
	
	private LinkedHashMap<String, SearchPredicateElement> predForSignature = new LinkedHashMap<String, SearchPredicateElement>();
	private ModuleMatch match;
	
	public SearchModuleElement(Object parent, String name, String visibility) {
		this.parent = parent;
		this.name = name;
		if (visibility == null || visibility.isEmpty()) {
			label = name;
		} else {
			label = name + " (" + visibility + ")";
		}
		if ("invisible".equalsIgnoreCase(visibility)) {
			visibilityCode = 1; 
		} else if ("sub".equalsIgnoreCase(visibility) || "descendant".equalsIgnoreCase(visibility)) {
			visibilityCode = 2; 
		} else if ("local".equalsIgnoreCase(visibility)) {
			visibilityCode = 3; 
		} else if ("super".equalsIgnoreCase(visibility) || "inherited".equalsIgnoreCase(visibility)) {
			visibilityCode = 4; 
		}
	}
	
	@Override
	public String getLabel() {
		return label;
	}
	
	public String getName() {
		return name;
	}
	
	@Override
	public Object[] getChildren() {
		return predForSignature.values().toArray();
	}

	@Override
	public boolean hasChildren() {
		return !predForSignature.isEmpty();
	}

	@Override
	public int compareTo(SearchModuleElement o) {
		int visibilityDifference = o.visibilityCode - this.visibilityCode;
		if (visibilityDifference != 0) {
			return visibilityDifference;
		} else {
			return name.compareTo(o.getName());
		}
	}

	@Override
	public void removeMatch(PrologMatch match) {
		String signature = getSignatureForMatch(match);
		if (predForSignature.containsKey(signature)) {
			SearchPredicateElement predicateElement = predForSignature.get(signature);
			predicateElement.removeMatch(match);
			if (!predicateElement.hasChildren()) {
				predForSignature.remove(signature);
			}
		}
	}

	@Override
	public void addMatch(PrologMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement searchPredicateElement = predForSignature.get(signature); 
		if (searchPredicateElement == null) {
			searchPredicateElement = new SearchPredicateElement(this, match.getModule(), match.getName(), match.getArity(), match.getProperties());
			predForSignature.put(signature, searchPredicateElement);
		}
		searchPredicateElement.addMatch(match);
	}
	
	private String getSignatureForMatch(PrologMatch match) {
		return match.getName() + match.getArity();
	}
	
	public void removeMatch(PredicateMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement searchPredicateElement = predForSignature.get(signature);
		searchPredicateElement.removeMatch(match);
		if (!searchPredicateElement.hasMatches()) {
			predForSignature.remove(signature);
		}
	}
	
	public void addMatch(PredicateMatch match) {
		String signature = getSignatureForMatch(match);
		SearchPredicateElement element = predForSignature.get(signature);
		if (element == null) {
			element = (SearchPredicateElement) match.getElement();
			element.setParent(this);
			predForSignature.put(signature, element);
		}
		element.addMatch(match);
	}

	private String getSignatureForMatch(PredicateMatch match) {
		return match.getName() + match.getArity();
	}
	
	@Override
	public Object getParent() {
		return parent;
	}
	
	public void setParent(Object parent) {
		this.parent = parent;
	}
	
	public int getLine() {
		if (match == null) {
			return -1;
		} else {
			return match.getOffset();
		}
	}
	
	public IFile getFile() {
		if (match == null) {
			return null;
		} else {
			return match.getFile();
		}
	}

	public void setMatch(ModuleMatch match) {
		this.match = match;
	}
	
	public ModuleMatch getMatch() {
		return match;
	}

}



