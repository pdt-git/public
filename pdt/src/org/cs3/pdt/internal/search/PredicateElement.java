package org.cs3.pdt.internal.search;

import java.util.List;

import org.cs3.pl.metadata.PrologElement;
import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a group of matches called from the same predicate.
 */
public class PredicateElement extends PrologElement{

	private static final long serialVersionUID = 8822257720982382862L;
	private IFile file;          // file that contains the selected literal (non-null)
	private String module;       // explicit module prefix of the selected literal (or null)
	private String predicateName; 
	private int arity;           
	
	public PredicateElement(IFile file, String module, String predicateName, int arity, List<String> properties) {
		super(module,predicateName,arity, properties);
		this.file = file;
		this.module = module;
		this.predicateName = predicateName;
		this.arity = arity;
	}
	
	public PredicateElement(IFile file, String module, String predicateName, int arity) {
		super(module, predicateName, arity);
		this.file = file;
		this.module = module;
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

	public String getModule() {
		return module;
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
