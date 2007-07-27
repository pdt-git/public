package org.cs3.pdt.internal.contentassistant;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.ICompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;

public class VariableCompletionProposal implements ICompletionProposal,
		Comparable<VariableCompletionProposal> {
	CompletionProposal target = null;

	public VariableCompletionProposal(String proposal, int begin, int len,
			int cursorPos, Image image, String displayString, IContextInformation contextInfo,
			String additionalInfo) {
		target=new CompletionProposal(proposal,begin,len,cursorPos,image,displayString,contextInfo,additionalInfo);
	}

	public void apply(IDocument document) {
		target.apply(document);

	}

	public String getAdditionalProposalInfo() {
		return target.getAdditionalProposalInfo();
	}

	public IContextInformation getContextInformation() {
		return target.getContextInformation();
	}

	public String getDisplayString() {
		return target.getDisplayString();
	}

	public Image getImage() {
		return target.getImage();
	}

	public Point getSelection(IDocument document) {
		return target.getSelection(document);
	}

	public int compareTo(VariableCompletionProposal o) {
		return target.getDisplayString().compareTo(o.target.getDisplayString());
	}

}
