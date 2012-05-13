package org.cs3.pdt.internal.structureElements;

import java.util.LinkedHashMap;
import java.util.Set;

import org.eclipse.core.resources.IFile;

public class FileTreeElement implements PDTSearchTreeElement {
	
	private LinkedHashMap<PDTMatch, SearchMatchElement> matchesToSearchElements = new LinkedHashMap<PDTMatch, SearchMatchElement>();
	private IFile file;
	private Object parent;
	
	public FileTreeElement(Object parent, IFile file) {
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
	
	public PDTMatch[] getOccurrences() {
		Set<PDTMatch> keySet = matchesToSearchElements.keySet();
		return keySet.toArray(new PDTMatch[keySet.size()]);
	}
	
	public int getNumberOfChildren() {
		return matchesToSearchElements.size();
	}
	
	public PDTMatch getFirstMatch() {
		if (matchesToSearchElements.isEmpty()) {
			return null;
		}
		PDTMatch firstMatch = null;
		int firstLine = Integer.MAX_VALUE;
		for (PDTMatch occurence : matchesToSearchElements.keySet()) {
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
	public void removeMatch(PDTMatch match) {
		SearchMatchElement removedElement = matchesToSearchElements.remove(match);
		if (removedElement != null) {
			removedElement.removeMatch(match);
		}
	}

	@Override
	public void addMatch(PDTMatch match) {
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

}
