/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

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
		char c = 'A';
		for (int i = 0; i < arity; i++) {
			if(i > 0) {
				buf.append(", ");
			}
			buf.append(c);
			if (c == '_' || c == 'Z') {
				c = '_';
			} else {
				c++;
			}
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


