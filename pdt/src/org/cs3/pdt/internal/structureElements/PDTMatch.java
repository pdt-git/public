/**
 * 
 */
package org.cs3.pdt.internal.structureElements;

import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class PDTMatch extends Match implements PDTTreeElement{

	private String module;
	private int line=-1;
	private IFile file;

	private boolean isLineLocation= false;
	private String visibility;
	private String declOrDef; 
	
	public PDTMatch(String visibility, Object element, IFile file, int offset, int length, String declOrDef) {
		super(element, UNIT_LINE, offset - 1, 1);
		this.declOrDef = declOrDef;
		this.file = file;
		this.visibility = visibility;
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

	public void setModule(String module) {
		this.module = module;
	}

	public String getModule() {
		return module;
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

	@Override
	public boolean hasChildren() {
		return false;
	}

	@Override
	public Object[] getChildren() {
		return new Object[0];
	}

	@Override
	public String getLabel() {
		StringBuffer label = new StringBuffer("Line ");
		label.append(Integer.toString(getLine()));
		label.append(" (");
		label.append(declOrDef);
		label.append(")");
		return label.toString();
	}
	
}