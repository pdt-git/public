package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;

public class FileTreeElement implements PDTTreeElement{
	private List<PDTMatch> occurrences= new ArrayList<PDTMatch>();
	private IFile file;
	
	public FileTreeElement(IFile file) {
		this.file = file;
	}
	
	public IFile getFile() {
		return file;
	}
	
	public void addChild(PDTMatch elem) {
		occurrences.add(elem);
	}

	@Override
	public boolean hasChildren() {
		return !(occurrences.isEmpty());
	}

	@Override
	public Object[] getChildren() {
		return occurrences.toArray();
	}
	
	public int getNumberOfChildren() {
		return occurrences.size();
	}
	
	public PDTMatch getFirstMatch() {
		if (occurrences.isEmpty()) {
			return null;
		}
		PDTMatch firstOccurrance = occurrences.get(0);
		int firstLine = firstOccurrance.getLine();
		for (PDTMatch occurence : occurrences) {
			int line = occurence.getLine();
			if (line < firstLine) {
				firstLine = line;
				firstOccurrance = occurence;
			}
		}
		return firstOccurrance;
	}

	@Override
	public String getLabel() {
		return file.getName();
	}

}
