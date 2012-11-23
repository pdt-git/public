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

import java.util.List;

import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.graphics.Image;

public class PredicateCompletionProposal extends AbstractCompletionProposal {

	private String functor;
	private int arity;
	private String label;
	private List<String> argNames;
	private String content;
	private String visibility;
	private boolean isBuiltin;
	
	public PredicateCompletionProposal(String functor, int arity, int prefixLength, String visibility, boolean isBuiltin, List<String> argNames, boolean addSingleQuote) {
		super(prefixLength, isBuiltin);
		this.functor = functor;
		this.arity = arity;
		this.argNames = argNames;
		this.visibility = visibility;
		this.isBuiltin = isBuiltin;
		label = functor + "/" + arity;
	}
	
	@Override
	public String getContent() {
		if (content == null) {
			content = (functor + (addSingleQuote ? "'" : "") + getArglist()).substring(prefixLength);
		}
		return content;
	}

	private String getArglist() {
		if (arity < 1) {
			return "";
		}
		
		StringBuffer buf = new StringBuffer("(");
		char c = 'A';
		int i = 0;
		
		if (argNames == null) {
			while (i < arity) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(c);
				if (c == '_' || c == 'Z') {
					c = '_';
				} else {
					c++;
				}
				i++;
			}
		} else {
			for (String argName : argNames) {
				if (i > 0) {
					buf.append(", ");
				}
				buf.append(argName);
				i++;
			}
		}
		buf.append(")");
		return buf.toString();
	}
	
	@Override
	public int getCursorPosition() {
		return getContent().length();
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public String getDescription() {
		return null;
	}
	
	@Override
	public Image getImage() {
		if (isBuiltin) {
			return ImageRepository.getImage(ImageRepository.PREDICATE_BUILTIN);
		} else {
			if (SearchConstants.VISIBILITY_PUBLIC.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PUBLIC);
			} else if (SearchConstants.VISIBILITY_PROTECTED.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PROTECTED);
			} else if (SearchConstants.VISIBILITY_PRIVATE.equals(visibility)) {
				return ImageRepository.getImage(ImageRepository.PREDICATE_PRIVATE);
			}
		}
		return null;
	}

}


