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

	public ComparableCompletionProposal(String proposal, int begin, int len,
			int cursorPos, Image image, String displayString,
			IContextInformation contextInfo, String additionalInfo) {
		target = new CompletionProposal(proposal, begin, len, cursorPos, image,
				displayString, contextInfo, additionalInfo);
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

}