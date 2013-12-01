package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;


public class SimpleCompletionProposal extends ComparableTemplateCompletionProposal {

	public SimpleCompletionProposal(IDocument document, String name, String description, String pattern, int offset, int length) {
		super(document, name, description, pattern, offset, length, ImageRepository.getImage(ImageRepository.TEMPLATE));
	}
	
	@Override
	public String getAdditionalProposalInfo() {
		return getTemplate().getPattern();
	}
	
	@Override
	protected int getPriority() {
		return PRIORITY_1;
	}
}
