/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.eclipse.search.ui.text.Match;

public class PrologMatch extends Match{

	public String type;

	public PrologMatch(Object element, int offset, int length) {
		super(element, offset, length);
	}
	
}