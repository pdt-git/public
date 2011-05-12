/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match{

	private String module;
	private int line=-1;

	private boolean isLineLocation= false; 
	
	public PrologMatch(Object element, int offset, int length) {
		super(element, UNIT_LINE, offset, length);
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
	
}