package org.cs3.pdt.internal.structureElements;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;

public class SearchModuleElement implements PDTTreeElement {
	private String name;
	private IFile file;
	private List<PDTMatch> elements = new ArrayList<PDTMatch>();
	List<SearchPredicateElement> predicates = new ArrayList<SearchPredicateElement>();
	
	public SearchModuleElement(String name) {
		this.name = name;
	}
	
	public String getLabel() {
		StringBuffer label = new StringBuffer(name);
		label.append(" (in ");
		label.append(file.getFullPath().toString());
		label.append(")");
		return label.toString();
	}
	
	public void addElement(PDTMatch elem) {
		elements.add(elem);
		
		SearchPredicateElement predicate = (SearchPredicateElement)elem.getElement();
		if (!predicates.contains(predicate)) {
			predicates.add(predicate);
		}
		
		file = predicate.getFile();
	}

	@Override
	public Object[] getChildren() {
		return  predicates.toArray();
	}

	@Override
	public boolean hasChildren() {
		return !predicates.isEmpty();
	}

}

