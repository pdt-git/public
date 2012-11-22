package org.cs3.pdt.console.internal.views;

import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.graphics.Image;

public class AtomCompletionProposal extends AbstractCompletionProposal {
	
	private String atom;
	private String content;
	
	public AtomCompletionProposal(String atom, int prefixLength, boolean addSingleQuote) {
		super(prefixLength, addSingleQuote);
		this.atom = atom;
	}
	
	@Override
	public String getContent() {
		if (content == null) {
			content = (atom + (addSingleQuote ? "'" : "")).substring(prefixLength);
		}
		return content;
	}

	@Override
	public int getCursorPosition() {
		return getContent().length();
	}

	@Override
	public String getLabel() {
		return atom;
	}

	@Override
	public String getDescription() {
		return null;
	}

	@Override
	public Image getImage() {
		return ImageRepository.getImage(ImageRepository.ATOM);
	}
	
}
