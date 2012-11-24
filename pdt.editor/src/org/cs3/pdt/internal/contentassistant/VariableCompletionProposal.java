package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;

public class VariableCompletionProposal extends ComparableTemplateCompletionProposal {

	private String variable;

	public VariableCompletionProposal(IDocument document, String variable, int offset, int length) {
		super(document, variable, variable, offset, length);
		this.variable = variable;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			return variable.compareTo(((VariableCompletionProposal) o).variable);
		} else {
			return -1;
		}
	}
	
	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
	}

}
