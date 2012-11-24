package org.cs3.pdt.console.internal.views;

import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.swt.graphics.Image;

public abstract class ComparableCompletionProposal implements IContentProposal, Comparable<ComparableCompletionProposal> {
	
	protected int prefixLength;
	protected boolean addSingleQuote;
	
	public ComparableCompletionProposal(int prefixLength, boolean addSingleQuote) {
		this.prefixLength = prefixLength;
		this.addSingleQuote = addSingleQuote;
	}
	
	public abstract Image getImage(); 
	
}
