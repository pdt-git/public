package org.cs3.pdt.console.internal.views;

import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.swt.graphics.Image;

public class AtomCompletionProposal extends ComparableCompletionProposal {
	
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

	@Override
	public int compareTo(ComparableCompletionProposal o) {
		if (o instanceof ModuleCompletionProposal || o instanceof PredicateCompletionProposal) {
			return 1;
		} else if (o instanceof AtomCompletionProposal) {
			return atom.compareTo(((AtomCompletionProposal) o).atom);
		} else {
			return -1;
		}
	}
	
}
