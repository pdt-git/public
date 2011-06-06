package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

class OutlineLabelProvider extends LabelProvider implements IColorProvider/*, IStyledLabelProvider*/ {
	@Override
	public String getText(Object element) {
		if(element instanceof PDTTreeElement) {
			return ((PDTTreeElement) element).getLabel();
		}
		return "";
	}

	@Override
	public Image getImage(Object element) {

		if(element instanceof Predicate) {
			Predicate prologPredicate = (Predicate) element;
			if (prologPredicate.isPublic()) {
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			} else if (prologPredicate.isPrivate()) {
				return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
			} else if (prologPredicate.isLocal()) {
				return ImageRepository.getImage(ImageRepository.PE_LOCAL);
			}
			return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
		}
		if(element instanceof ModuleElement) {
			//			ModuleOutlineElement module = (ModuleOutlineElement)element;
//			if(module.hasChildren())
				return ImageRepository.getImage(ImageRepository.PACKAGE);
		}
		if (element instanceof PredicateOccuranceElement) {
			ISharedImages sharedImagaes = PlatformUI.getWorkbench().getSharedImages();
			return setCategoryImage(sharedImagaes);
		}
		return null;
	}

	private Image setCategoryImage(ISharedImages sharedImagaes) {
		return sharedImagaes.getImage(ISharedImages.IMG_DEF_VIEW);
	}

	@Override
	public Color getForeground(Object element) {
		if(element instanceof Predicate) {
			Predicate predicate = (Predicate) element;
			if(predicate.isDynamic()) {
			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLUE);
			}
		}	
		return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLACK);
	}

	@Override
	public Color getBackground(Object element) {
//		OutlinePredicate prologPredicate = (OutlinePredicate) element;
//		if(prologPredicate.isDynamic()) {
//			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_GRAY);
//		}
		return null;
	}

}