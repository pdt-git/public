package org.cs3.pdt.internal.contentassistant;

import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.Region;
import org.eclipse.jface.text.templates.DocumentTemplateContext;
import org.eclipse.jface.text.templates.Template;
import org.eclipse.jface.text.templates.TemplateContextType;
import org.eclipse.jface.text.templates.TemplateProposal;

public abstract class ComparableTemplateCompletionProposal extends TemplateProposal implements Comparable<ComparableTemplateCompletionProposal> {

	private static final String contextTypeId = "MyContextType";
	private String displayString;
	
	public ComparableTemplateCompletionProposal(IDocument document, String insertion, String displayString, int offset, int length) {
		super(new Template(insertion, "InsertMe", contextTypeId , insertion, true), new DocumentTemplateContext(new TemplateContextType(contextTypeId), document, offset, length), new Region(offset, length), null);
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
