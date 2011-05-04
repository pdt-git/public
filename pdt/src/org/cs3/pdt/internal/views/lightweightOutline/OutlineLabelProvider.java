package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.PlatformUI;

class OutlineLabelProvider extends LabelProvider implements IColorProvider{//, IStyledLabelProvider {
	@Override
	public String getText(Object element) {
		OutlinePredicate prologPredicate = (OutlinePredicate)element;
		return prologPredicate.getName()  +"/" + prologPredicate.getArity();
	}

	@Override
	public Image getImage(Object element) {
		OutlinePredicate prologPredicate = (OutlinePredicate) element;

		if (prologPredicate.isPublic()) {
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		}
		return ImageRepository.getImage(ImageRepository.PE_HIDDEN);
	}

@Override
public Color getForeground(Object element) {
	OutlinePredicate prologPredicate = (OutlinePredicate) element;
	if(prologPredicate.isMultifile()) {
		return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLUE);
	}
	return null;
}

@Override
public Color getBackground(Object element) {
	OutlinePredicate prologPredicate = (OutlinePredicate) element;
	if(prologPredicate.isDynamic()) {
		return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_GRAY);
	}
	return null;
}

}