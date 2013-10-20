/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Andreas Becker
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.console.internal.views;

import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.graphics.Image;

public class ModuleCompletionProposal extends ComparableCompletionProposal {
	
	private String module;
	private String content;
	
	public ModuleCompletionProposal(String module, int prefixLength, boolean addSingleQuote) {
		super(prefixLength, addSingleQuote);
		this.module = module;
	}
	
	@Override
	public String getContent() {
		if (content == null) {
			content = (module + (addSingleQuote ? "'" : "")).substring(prefixLength);
		}
		return content;
	}

	@Override
	public int getCursorPosition() {
		return getContent().length();
	}

	@Override
	public String getLabel() {
		return module;
	}

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.ENTITY);
	}

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof ModuleCompletionProposal) {
			return module.compareTo(((ModuleCompletionProposal) o).module);
		} else if (o instanceof PredicateCompletionProposal) {
			return module.compareTo(((PredicateCompletionProposal) o).getSignature());
		} else {
			return -1;
		}
	}
	
}
