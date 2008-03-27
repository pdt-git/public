package org.cs3.pdt.internal.contentassistant;

import org.eclipse.jface.text.contentassist.CompletionProposal;
import org.eclipse.jface.text.contentassist.IContextInformation;
import org.eclipse.swt.graphics.Image;

public class VariableCompletionProposal extends ComparableCompletionProposal{
	
	public VariableCompletionProposal(String proposal, int begin, int len,
			int cursorPos, Image image, String displayString, IContextInformation contextInfo,
			String additionalInfo) {
		super(proposal,begin,len,cursorPos,image,displayString,contextInfo,additionalInfo);
	}

	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			String displayString = target.getDisplayString();
			VariableCompletionProposal variableCompletionProposal = ((VariableCompletionProposal)o);
			CompletionProposal target2 = variableCompletionProposal.target;
			return displayString.compareTo(target2.getDisplayString());
		}
		return 0;
	}

}
