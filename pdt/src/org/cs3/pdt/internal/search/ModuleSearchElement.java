package org.cs3.pdt.internal.search;

import java.util.ArrayList;
import java.util.List;

import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;
import org.eclipse.core.resources.IFile;

public class ModuleSearchElement implements PDTTreeElement {
	private String name;
	private IFile file;
	private List<PrologMatch> element = new ArrayList<PrologMatch>();
//	Set<FileTreeElement> files = new HashSet<FileTreeElement>();
	List<PredicateElement> predicates = new ArrayList<PredicateElement>();
	
	public ModuleSearchElement(String name) {
		this.name = name;
	}
	
	public String getLabel() {
		StringBuffer label = new StringBuffer(name);
		label.append(" (");
		label.append(file.getFullPath().toString());
//		label.append(file.getLocation());
		label.append(")");
		return label.toString();
	}
	
	public void addElement(PrologMatch elem) {
		element.add(elem);
		
		PredicateElement predicate = (PredicateElement)elem.getElement();
		predicates.add(predicate);
		
		file = predicate.getFile();

//		for (FileTreeElement fileElement : files) {
//			if (fileElement.getFile().equals(file)) {
//				found = true;
//				fileElement.addChild((PredicateElement)elem.getElement());
//			}
//		}
//		if (!found) {
//			FileTreeElement fileElement = new FileTreeElement(file);
//			files.add(fileElement);
//			fileElement.addChild((PredicateElement)elem.getElement());
//		}
	}

	@Override
	public Object[] getChildren() {
		PredicateElement[] unsortedFiles = predicates.toArray(new PredicateElement[predicates.size()]);
//		FileTreeElement[] unsortedFiles = files.toArray(new FileTreeElement[files.size()]);
		return unsortedFiles;
		
//		return PDTCoreUtils.sortFileSet(unsortedFiles);
	}

	@Override
	public boolean hasChildren() {
		return !element.isEmpty();
	}

}

