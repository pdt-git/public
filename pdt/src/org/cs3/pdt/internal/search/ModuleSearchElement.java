package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;
import org.eclipse.core.resources.IFile;

public class ModuleSearchElement implements PDTTreeElement {
	private String name;
	private List<PrologMatch> element = new ArrayList<PrologMatch>();
	Set<IFile> files = new HashSet<IFile>();
	
	public ModuleSearchElement(String name) {
		this.name = name;
	}
	
	public String getLabel() {
		return name;
	}
	
	public void addElement(PrologMatch elem) {
		element.add(elem);
		files.add(((PredicateElement)elem.getElement()).getFile());
	}

	@Override
	public IFile[] getChildren() {
		IFile[] unsortedFiles = files.toArray(new IFile[files.size()]);
		return PDTCoreUtils.sortFileSet(unsortedFiles);
	}

	@Override
	public boolean hasChildren() {
		return !element.isEmpty();
	}

}

