/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;
import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match implements PDTTreeElement{

	private String module;
	private int line=-1;
	private String kind;

	private boolean isLineLocation= false; 
	
	public PrologMatch(Object element, int offset, int length, String kind) {
		super(element, UNIT_LINE, offset, length);
		this.kind = kind;
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
		StringBuffer label = new StringBuffer("Line: ");
		label.append(Integer.toString(getLine()));
		label.append(" (");
		label.append(kind);
		label.append(")");
		return label.toString();
	}
	
}