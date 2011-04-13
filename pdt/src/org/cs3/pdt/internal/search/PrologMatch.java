/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match{

	public String type;
	private int line=-1;

	private boolean isLineLocation= false; 
	
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

	public PrologMatch(Object element, int offset, int length) {
		super(element, offset, length);
	}
	
}