package org.cs3.pdt.internal.structureElements;

import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Vector;

import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;

/**
 * used in prolog searches and outlines to represent a predicate.
 */
public class SearchPredicateElement extends Predicate implements PDTSearchTreeElement {

	private static final long serialVersionUID = 8822257720982382862L;
	
	private LinkedHashMap<IFile, FileTreeElement> fileToFileTreeElement = new LinkedHashMap<IFile, FileTreeElement>();
	private Object parent;
	
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
		int sum = 0;
		for (FileTreeElement e : fileToFileTreeElement.values()) {
			sum += e.getNumberOfChildren();
		}
		return sum;
	}
	
	public PDTMatch getFirstOccurrence() {
		if (fileToFileTreeElement.values().isEmpty()) {
			return null;
		} else {
			for (FileTreeElement e : fileToFileTreeElement.values()) {
				return e.getFirstMatch();
			}
			return null;
		}
	}
	
	public PDTMatch[] getOccurrences() {
		HashSet<PDTMatch> matches = new HashSet<PDTMatch>();
		for (FileTreeElement e : fileToFileTreeElement.values()) {
			for (PDTMatch m : e.getOccurrences()) {
				matches.add(m);
			}
		}
		return matches.toArray(new PDTMatch[matches.size()]);
	}
	
	public FileTreeElement[] getFileTreeElements() {
		Collection<FileTreeElement> values = fileToFileTreeElement.values();
		return values.toArray(new FileTreeElement[values.size()]);
	}

	@Override
	public void removeMatch(PDTMatch match) {
		IFile file = match.getFile();
		if (fileToFileTreeElement.containsKey(file)) {
			FileTreeElement fileTreeElement = fileToFileTreeElement.get(file);
			fileTreeElement.removeMatch(match);
			if (!fileTreeElement.hasChildren()) {
				fileToFileTreeElement.remove(file);
			}
		}
	}

	@Override
	public void addMatch(PDTMatch match) {
		FileTreeElement fileTreeElement = fileToFileTreeElement.get(match.getFile());
		if (fileTreeElement == null) {
			fileTreeElement = new FileTreeElement(this, match.getFile());
			fileToFileTreeElement.put(fileTreeElement.getFile(), fileTreeElement);
		}
		fileTreeElement.addMatch(match);
		match.setPredicateElement(this);
	}

	@Override
	public Object getParent() {
		return parent;
	}

}
