package org.cs3.pdt.console.internal.views;

import org.cs3.pdt.console.internal.ImageRepository;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.graphics.Image;

public class ConsoleCompletionLabelProvider extends LabelProvider implements ILabelProvider {

	@Override
	public Image getImage(Object element) {
		if (element instanceof CompletionProposal) {
			return ImageRepository.getImage(ImageRepository.PREDICATE_PUBLIC);
		}
		return null;
	}

	@Override
	public String getText(Object element) {
		if (element instanceof CompletionProposal) {
			return ((CompletionProposal) element).getLabel();
		}
		return super.getText(element);
	}

}
