package org.cs3.pdt.internal.search;

import java.util.List;
import java.util.Vector;

import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches to represent a group of matches called from the same predicate.
 */
public class PredicateElement extends Predicate{

	private static final long serialVersionUID = 8822257720982382862L;
	private IFile file;          // file that contains the selected literal (non-null)        
	
	public PredicateElement(IFile file, String module, String predicateName, int arity, List<String> properties) {
		super(module,predicateName,arity, properties);
		this.file = file;
	}
	
	public PredicateElement(IFile file, String module, String predicateName, int arity) {
		super(module, predicateName, arity, new Vector<String>());
		this.file = file;
	}
	

	public IFile getFile() {
		return file;
	}

}
