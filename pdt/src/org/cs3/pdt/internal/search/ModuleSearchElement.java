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
	Set<FileTreeElement> files = new HashSet<FileTreeElement>();
	
	public ModuleSearchElement(String name) {
		this.name = name;
	}
	
	public String getLabel() {
		return name;
	}
	
	public void addElement(PrologMatch elem) {
		element.add(elem);
		IFile file = ((PredicateElement)elem.getElement()).getFile();
		boolean found = false;
		for (FileTreeElement fileElement : files) {
			if (fileElement.getFile().equals(file)) {
				found = true;
				fileElement.addChild((PredicateElement)elem.getElement());
			}
		}
		if (!found) {
			files.add(new FileTreeElement(file));
		}
	}

	@Override
	public Object[] getChildren() {
		FileTreeElement[] unsortedFiles = files.toArray(new FileTreeElement[files.size()]);
		return unsortedFiles;
		
//		return PDTCoreUtils.sortFileSet(unsortedFiles);
	}

	@Override
	public boolean hasChildren() {
		return !element.isEmpty();
	}

}

