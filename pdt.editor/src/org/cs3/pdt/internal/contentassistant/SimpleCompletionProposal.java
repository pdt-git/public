package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;


public class SimpleCompletionProposal extends ComparableTemplateCompletionProposal {

	private String insertion;

	public SimpleCompletionProposal(IDocument document, String insertion, String displayString, int offset, int length) {
		super(document, insertion, displayString, offset, length);
		this.insertion = insertion;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof SimpleCompletionProposal || o instanceof ModuleCompletionProposal) {
			return getDisplayString().compareTo(o.getDisplayString());
		} else if (o instanceof AtomCompletionProposal) {
			return -1;
		}
		return -1;
	}
	
	@Override
	public String getAdditionalProposalInfo() {
		return insertion;
	}
	
	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.TEMPLATE);
	}

}
