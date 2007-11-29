package org.cs3.pdt.internal.search;

import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a group of matches called from the same predicate.
 */
public class PredicateElement {
	public IFile file;
	public String label;
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof PredicateElement){
			PredicateElement other = (PredicateElement) obj;
			return other.file.equals(this.file) && other.label.equals(this.label);
		}
		return false;
	}
	@Override
	public int hashCode() {	
		return label.hashCode();
	}
}
