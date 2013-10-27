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

package org.cs3.pdt.console.internal.views.completion;

import java.util.List;

import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;

public class PredicateCompletionProposal extends ComparableCompletionProposal {

	private String functor;
	private int arity;
	private String label;
	private List<String> argNames;
	private String term;
	private String indicator;
	private String visibility;
	private boolean isBuiltin;
	private int lastStateMask = -1;
	
	public PredicateCompletionProposal(String module, String functor, int arity, int prefixLength, String visibility, boolean isBuiltin, List<String> argNames, boolean addSingleQuote) {
		super(prefixLength, addSingleQuote);
		this.functor = functor;
		this.arity = arity;
		this.argNames = argNames;
		this.visibility = visibility;
		this.isBuiltin = isBuiltin;
		if (module == null) {
			label = functor + "/" + arity;
		} else {
			label = functor + "/" + arity + " - " + module;
		}
		term = (functor + (addSingleQuote ? "'" : "") + getArglist()).substring(prefixLength);
		indicator = (functor + "/" + arity).substring(prefixLength);
	}
	
	@Override
	public String getContent(int stateMask) {
		lastStateMask = stateMask;
		if ((stateMask & SWT.CTRL) != 0) {
			return indicator;
		} else if ((stateMask & SWT.SHIFT) != 0) {
			return functor.substring(prefixLength);
		} else {
			return term;
		}
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
		return getContent(lastStateMask).length();
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
	
	String getSignature() {
		return functor + "/" + arity;
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof PredicateCompletionProposal) {
			return getSignature().compareTo(((PredicateCompletionProposal) o).getSignature());
		} else if (o instanceof ModuleCompletionProposal) {
			return getSignature().compareTo(o.getLabel());
		} else {
			return -1;
		}
	}

}


