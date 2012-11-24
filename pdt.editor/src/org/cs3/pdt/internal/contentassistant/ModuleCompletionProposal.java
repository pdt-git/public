package org.cs3.pdt.internal.contentassistant;

import org.cs3.pdt.common.search.SearchConstants;
import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.graphics.Image;

@SuppressWarnings("unused")
public class ModuleCompletionProposal extends ComparableTemplateCompletionProposal {

	private String module;

	public ModuleCompletionProposal(IDocument document, String module, int offset, int length) {
		super(document, module, module, offset, length);
		this.module = module;
	}

	@Override
	public int compareTo(ComparableTemplateCompletionProposal o) {
		if (o instanceof VariableCompletionProposal) {
			return -1;
		} else if (o instanceof ModuleCompletionProposal) {
			return getDisplayString().compareTo(o.getDisplayString());
		} else if (o instanceof PredicateCompletionProposal) {
			return getDisplayString().compareTo(((PredicateCompletionProposal) o).getSignature());
		} else {
			return 1;
		}
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.PACKAGE);
	}
	
}
