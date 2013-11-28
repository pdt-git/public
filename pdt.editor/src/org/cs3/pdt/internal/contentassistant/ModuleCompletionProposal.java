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

package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;

public class ModuleCompletionProposal extends ComparableTemplateCompletionProposal {

	private String module;

	public ModuleCompletionProposal(IDocument document, String module, int offset, int length) {
		super(document, module, module, offset, length);
		this.module = module;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			return 1;
		} else if (o instanceof ModuleCompletionProposal) {
			return module.compareTo(((ModuleCompletionProposal) o).module);
		} else if (o instanceof PredicateCompletionProposal) {
			return module.compareTo(((PredicateCompletionProposal) o).getSignature());
		} else if (o instanceof SimpleCompletionProposal) {
			return module.compareTo(o.getDisplayString());
		} else {
			return -1;
		}
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.PACKAGE);
	}
	
}
