package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches and outlines to represent a predicate.
 */
public class SearchPredicateElement extends Predicate implements PDTTreeElement{

	private static final long serialVersionUID = 8822257720982382862L;
	private IFile file;          // file that contains the selected literal (non-null)       
	private PDTTreeElement parent;
	private List<PDTMatch> occurences = new ArrayList<PDTMatch>();
	
	public SearchPredicateElement(IFile file, String module, String predicateName, int arity, List<String> properties) {
		super(module,predicateName,arity, properties);
		this.file = file;
	}
	
	public SearchPredicateElement(IFile file, String module, String predicateName, int arity) {
		super(module, predicateName, arity, new Vector<String>());
		this.file = file;
	}
	
	public IFile getFile() {
		return file;
	} 
	
	public void addOccurence(PDTMatch occurance) {
		occurences.add(occurance);
	}

	@Override
	public boolean hasChildren() {
		return !occurences.isEmpty();
	}

	@Override
	public Object[] getChildren() {
		return occurences.toArray();
	}
	
	@Override
	public String getLabel() {
		return getFunctor() + "/" + getArity();
	}
	
	public int numberOfOccurences() {
		return occurences.size();
	}
	
	public PDTMatch getFirstOccurence() {
		PDTMatch firstOccurance = occurences.get(0);
		int firstLine = firstOccurance.getLine();
		for (PDTMatch occurence : occurences) {
			int line = occurence.getLine();
			if (line < firstLine) {
				firstLine = line;
				firstOccurance = occurence;
			}
		}
		return firstOccurance;
	}

	public void setParent(PDTTreeElement parent) {
		this.parent = parent;
	}

	public PDTTreeElement getParent() {
		return parent;
	}
	
}
