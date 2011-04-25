package org.cs3.pdt.internal.search;

import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a group of matches called from the same predicate.
 */
public class PredicateElement {
	private IFile file;
	private String type;
	private String predicateName;
	private int arity;
	
	public PredicateElement(IFile file, String type, String predicateName, int arity) {
		this.file = file;
		this.type = type;
		this.predicateName = predicateName;
		this.arity = arity;
	}
	
	
	@Override
	public boolean equals(Object obj) {
		if (obj instanceof PredicateElement){
			PredicateElement other = (PredicateElement) obj;
			return other.getFile().equals(this.getFile()) && other.getLabel().equals(this.getLabel());
		}
		return false;
	}
	@Override
	public int hashCode() {	
		return getLabel().hashCode();
	}

	
	public String getLabel() {
		return (String) predicateName + "/" + arity;
	}

	public IFile getFile() {
		return file;
	}

	public String getType() {
		return type;
	}

	public String getPredicateName() {
		return predicateName;
	}

	/**
	 * The arity of the predicate to look up at the line offset
	 * @return
	 */
	public int getArity() {
		return arity;
	}
}
