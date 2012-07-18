package org.cs3.pdt.console.internal.views;

import org.eclipse.jface.fieldassist.IContentProposal;

public class CompletionProposal implements IContentProposal{

//	private String functor;
	private int arity;
//	private int prefixLength;
	private String label;
	private String content;
	
	public CompletionProposal(String functor, int arity, int prefixLength) {
//		this.functor = functor;
		this.arity = arity;
//		this.prefixLength = prefixLength;
		label = functor + "/" + arity;
		content = (functor + getArglist()).substring(prefixLength);
	}
	
	@Override
	public String getContent() {
		return content;
	}

	private String getArglist() {
		if (arity < 1) {
			return "";
		}
		
		StringBuffer buf = new StringBuffer("(");
		for (int i = 0; i < arity; i++) {
			if(i > 0) {
				buf.append(", ");
			}
			buf.append("_");
		}
		buf.append(")");
		return buf.toString();
	}
	
	@Override
	public int getCursorPosition() {
		return content.length();
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public String getDescription() {
		return null;
	}

}
