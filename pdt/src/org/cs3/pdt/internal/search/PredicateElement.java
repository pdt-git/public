package org.cs3.pdt.internal.search;

import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a group of matches called from the same predicate.
 */
public class PredicateElement {
	private IFile file;
	private String label;
	private String type;
	private String predicateName;
	private int arity;
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
	public void setLabel(String label) {
		this.label = label;
	}
	public String getLabel() {
		return label;
	}
	public void setFile(IFile file) {
		this.file = file;
	}
	public IFile getFile() {
		return file;
	}
	public void setType(String type) {
		this.type = type;
	}
	public String getType() {
		return type;
	}
	public void setPredicateName(String name) {
		this.predicateName = name;
	}
	public String getPredicateName() {
		return predicateName;
	}
	public void setArity(int arity) {
		this.arity = arity;
	}
	/**
	 * The arity of the predicate to look up at the line offset
	 * @return
	 */
	public int getArity() {
		return arity;
	}
}
