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

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContext;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateProposal;
import org.eclipse.swt.graphics.Image;

public abstract class ComparableTemplateCompletionProposal extends TemplateProposal implements Comparable<ComparableTemplateCompletionProposal> {

	protected static final String contextTypeId = "MyContextType";
	private String displayString;
	
	public ComparableTemplateCompletionProposal(IDocument document, String insertion, String displayString, int offset, int length) {
		super(new Template(insertion, "InsertMe", contextTypeId , insertion, true), new DocumentTemplateContext(new TemplateContextType(contextTypeId), document, offset, length), new Region(offset, length), null);
		this.displayString = displayString;
	}

	public ComparableTemplateCompletionProposal(Template template, TemplateContext context, IRegion region, String displayString) {
		super(template, context, region, null);
		this.displayString = displayString;
	}

	@Override
	public CharSequence getPrefixCompletionText(IDocument document, int completionOffset) {
		return getDisplayString();
	}

	@Override
	public int getPrefixCompletionStart(IDocument document, int completionOffset) {
		return completionOffset;
	}

	
	@Override
	public String getDisplayString() {
		return displayString;
	}

	@Override
	public String getAdditionalProposalInfo() {
		return null;
	}
}
