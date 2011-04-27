package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.core.PDTCoreUtils;
import org.eclipse.core.resources.IFile;

public class ModuleSearchDummy {
	private String name;
	private List<PrologMatch> element = new ArrayList<PrologMatch>();
	Set<IFile> files = new HashSet<IFile>();
	
	public ModuleSearchDummy(String name) {
		this.name = name;
	}
	
	public String getLabel() {
		return name;
	}
	
	public void addElement(PrologMatch elem) {
		element.add(elem);
		files.add(((PredicateElement)elem.getElement()).getFile());
	}
	
	public IFile[] getFiles() {
		IFile[] unsortedFiles = files.toArray(new IFile[files.size()]);
		return PDTCoreUtils.sortFileSet(unsortedFiles);
	}
	
}

