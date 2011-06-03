/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.views.lightweightOutline.PDTTreeElement;
import org.cs3.pl.metadata.PrologElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

public class PrologSearchLabelProvider implements ILabelProvider {
	/**
	 * 
	 */
	private final PrologSearchResultPage prologSearchResultPage;

	/**
	 * @param prologSearchResultPage
	 */
	PrologSearchLabelProvider(PrologSearchResultPage prologSearchResultPage) {
		this.prologSearchResultPage = prologSearchResultPage;
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof IFile) {
			return ImageRepository.getImage(ImageRepository.FILE);
		} else if (element instanceof PrologMatch) {
			ISharedImages sharedImagaes = PlatformUI.getWorkbench().getSharedImages();
			return setCategoryImage(sharedImagaes);
		} else if(element instanceof ModuleSearchElement){
			//return ImageRepository.getImage(ImageRepository.PE_MODULE);
			return ImageRepository.getImage(ImageRepository.PACKAGE);
		} else if (element instanceof SearchResultCategory) {
			ISharedImages sharedImagaes = PlatformUI.getWorkbench().getSharedImages();
			return sharedImagaes.getImage(ISharedImages.IMG_OBJ_ADD);
		} else if(element instanceof PredicateElement){
			return setPredicateImage(element);
		} 
		return null;
	}

	private Image setPredicateImage(Object element) {
		PrologElement pe = (PrologElement) element;
		if (pe.isPublic()) {
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		} 
		if (pe.isPrivate()) {
			return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
		} if (pe.isLocal()) {
			return ImageRepository.getImage(ImageRepository.PE_LOCAL);
		}
		return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
	}

	private Image setCategoryImage(ISharedImages sharedImagaes) {
		return sharedImagaes.getImage(ISharedImages.IMG_DEF_VIEW);
	}

	@Override
	public String getText(Object element) {
		if(element instanceof PredicateElement){
			PredicateElement pe = ((PredicateElement)element);
			String label = pe.getSignature();
			int count = this.prologSearchResultPage.getDisplayedMatchCount(element);
			String plural = (count==1)?"":"es";
			return label+ " (" + count +" match"+plural+")";
		} else if(element instanceof PDTTreeElement){
			return ((PDTTreeElement)element).getLabel();
		} else if (element instanceof SearchResultCategory) {
			return ((SearchResultCategory)element).getLabel();
//		} else if (element instanceof ModuleSearchElement) {
//			return ((ModuleSearchElement)element).getLabel();
//		} else if(element instanceof PrologMatch) {
//			return (((PrologMatch)element).getLabel());
		}
		return "no label";
	}

	@Override
	public void addListener(ILabelProviderListener listener) {
	}

	@Override
	public void dispose() {
	}

	@Override
	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	@Override
	public void removeListener(ILabelProviderListener listener) {
	}
}