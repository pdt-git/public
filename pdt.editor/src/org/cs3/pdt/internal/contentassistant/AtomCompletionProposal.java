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

public class AtomCompletionProposal extends ComparableTemplateCompletionProposal {

	private String atom;

	public AtomCompletionProposal(IDocument document, String atom, int offset, int length) {
		super(document, atom, atom, offset, length);
		this.atom = atom;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof PredicateCompletionProposal || o instanceof ModuleCompletionProposal || o instanceof VariableCompletionProposal) {
			return 1;
		} else if (o instanceof AtomCompletionProposal){
			return atom.compareTo(((AtomCompletionProposal) o).atom);
		} else {
			return -1;
		}
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.PE_ATOM);
	}
	
}
