/**
 * 
 */
package org.cs3.pdt.internal.structureElements;

import org.eclipse.core.resources.IFile;
import org.eclipse.search.ui.text.Match;

public class PDTMatch extends Match implements PDTTreeElement{

	private String module;
	private int line=-1;
	private String kind;
	private IFile file;

	private boolean isLineLocation= false;
	private String visibility; 
	
	public PDTMatch(String visibility, Object element, IFile file, int offset, int length, String kind) {
		super(element, UNIT_LINE, offset, length);
		this.kind = kind;
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
		return null;
	}

	@Override
	public String getLabel() {
		StringBuffer label = new StringBuffer("Line ");
		label.append(Integer.toString(getLine()));
		label.append(" (");
		label.append(kind);
		label.append(")");
		return label.toString();
	}
	
}