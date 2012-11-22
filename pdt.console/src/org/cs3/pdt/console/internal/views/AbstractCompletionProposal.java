package org.cs3.pdt.console.internal.views;

import org.eclipse.jface.fieldassist.IContentProposal;
import org.eclipse.swt.graphics.Image;

public abstract class AbstractCompletionProposal implements IContentProposal {
	
	protected int prefixLength;
	protected boolean addSingleQuote;
	
	public AbstractCompletionProposal(int prefixLength, boolean addSingleQuote) {
		this.prefixLength = prefixLength;
		this.addSingleQuote = addSingleQuote;
	}
	
	public abstract Image getImage(); 
	
}
