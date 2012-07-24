/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.structureElements;

import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match{

	private String module;
	private int line = -1;
	private IFile file;

	private boolean isLineLocation= false;
	private String visibility;
	private String declOrDef;
	private SearchPredicateElement predicateElement;
	private String name;
	private int arity;
	private List<String> properties; 
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int line, String declOrDef) {
		super(searchMatchElement, UNIT_LINE, line - 1, 1);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
		this.line = line;
		isLineLocation = true;
	}
	
	public PrologMatch(SearchMatchElement searchMatchElement, String visibility, String module, String name, int arity, List<String> properties, IFile file, int offset, int length, String declOrDef) {
		super(searchMatchElement, UNIT_CHARACTER, offset, length);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
		this.module = module;
		this.name = name;
		this.arity = arity;
		this.properties = properties;
		isLineLocation = false;
	}
	
	public int getLine() {
		return line;
	}

	public void setLine(int line) {
		this.isLineLocation=true;
		this.line = line;
	}

	public boolean isLineLocation() {
		return isLineLocation;
	}

	public String getModule() {
		return module;
	}
	
	public String getName() {
		return name;
	}
	
	public int getArity() {
		return arity;
	}
	
	public List<String> getProperties() {
		return properties;
	}
	
	public String getDeclOrDef() {
		return declOrDef;
	}
	
	public String getVisibility() {
		return visibility;
	}
	
	public IFile getFile() {
		return file;
	}
	
	public void setPredicateElement(SearchPredicateElement element) {
		predicateElement = element;
	}
	
	public SearchPredicateElement getPredicateElement() {
		if (predicateElement == null) {
			return (SearchPredicateElement) ((SearchFileTreeElement)((SearchMatchElement)getElement()).getParent()).getParent();
		} else {
			return predicateElement;
		}
	}

	public String getLabel() {
		StringBuffer label = new StringBuffer("Line ");
		label.append(Integer.toString(getLine()));
		label.append(" (");
		label.append(declOrDef);
		label.append(")");
		return label.toString();
	}
	
}