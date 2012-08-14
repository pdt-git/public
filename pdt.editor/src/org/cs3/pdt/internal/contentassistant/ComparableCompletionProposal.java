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

package org.cs3.pdt.internal.contentassistant;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

/**
 * ComparableCompletionProposal encapsulates a CompletionProposal to make it comparable.
 * This is necessary because CompletionProposal is a final class.
 * @author stoewe
 *
 */
public abstract class ComparableCompletionProposal implements Comparable<ComparableCompletionProposal>,ICompletionProposal{
	CompletionProposal target = null;
	private String doc;
	protected Image image;

	public ComparableCompletionProposal(String proposal, int begin, int len,
			int cursorPos, Image image, String displayString,
			IContextInformation contextInfo, String additionalInfo) {
		target = new CompletionProposal(proposal, begin, len, cursorPos, image,
				displayString, contextInfo, additionalInfo);
		this.image = image;
	}
	
	@Override
	public void apply(IDocument document) {
		target.apply(document);
	}

	@Override
	public String getAdditionalProposalInfo() {
		return target.getAdditionalProposalInfo();
	}

	@Override
	public IContextInformation getContextInformation() {
		return target.getContextInformation();
	}

	@Override
	public String getDisplayString() {
		return target.getDisplayString();
	}

	@Override
	public Image getImage() {
		return target.getImage();
	}

	@Override
	public Point getSelection(IDocument document) {
		return target.getSelection(document);
	}
	
	@Override
	public abstract int compareTo(ComparableCompletionProposal o);

	public void setDocumentation(String doc) {
		this.doc = doc;
		
	}

	public String getDocumentation() {
		return doc;
	}

}


