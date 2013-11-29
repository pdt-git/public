package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DocumentEvent;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;


public class SimpleCompletionProposal extends ComparableTemplateCompletionProposal {

	private String prefixToMatch;
	private String insertion;

	public SimpleCompletionProposal(IDocument document, String insertion, String prefixToMatch, String displayString, int offset, int length) {
		super(document, insertion, displayString, offset, length);
		this.prefixToMatch = prefixToMatch;
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

	@Override
	public boolean validate(IDocument document, int offset, DocumentEvent event) {
		try {
			int replaceOffset= getReplaceOffset();
			if (offset >= replaceOffset) {
				String content= document.get(replaceOffset, offset - replaceOffset);
				return prefixToMatch.startsWith(content.toLowerCase());
			}
		} catch (BadLocationException e) {
			// concurrent modification - ignore
		}
		return false;
	}
}
