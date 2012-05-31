/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.structureElements.PrologTreeElement;
import org.cs3.pdt.internal.structureElements.SearchFileTreeElement;
import org.cs3.pdt.internal.structureElements.SearchMatchElement;
import org.cs3.pdt.internal.structureElements.SearchModuleElement;
import org.cs3.pdt.internal.structureElements.SearchPredicateElement;
import org.cs3.pl.common.ExternalPrologFilesProjectUtils;
import org.cs3.pl.metadata.PrologElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

public class PrologSearchLabelProvider implements ILabelProvider {

	PrologSearchLabelProvider() {
	}

	@Override
	public Image getImage(Object element) {
		if (element instanceof IFile) {
			return ImageRepository.getImage(ImageRepository.FILE);
		} else if (element instanceof SearchMatchElement) {
			return ImageRepository.getImage(ImageRepository.SEARCH_MATCH);
//			ISharedImages sharedImagaes = PlatformUI.getWorkbench().getSharedImages();
//			return setCategoryImage(sharedImagaes);
		} else if(element instanceof SearchModuleElement){
			//return ImageRepository.getImage(ImageRepository.PE_MODULE);
			return ImageRepository.getImage(ImageRepository.PACKAGE);
		} else if(element instanceof SearchPredicateElement){
			return setPredicateImage(element);
		} else if(element instanceof SearchFileTreeElement){
			SearchFileTreeElement fileTreeElement = (SearchFileTreeElement) element;
			if (ExternalPrologFilesProjectUtils.isExternalFile(fileTreeElement.getFile())) {
				return ImageRepository.getImage(ImageRepository.PROLOG_FILE_EXTERNAL);
			} else {
				return ImageRepository.getImage(ImageRepository.PROLOG_FILE_CONSULTED);
			}
		} 
		return null;
	}

	private Image setPredicateImage(Object element) {
		PrologElement pe = (PrologElement) element;
		if (pe.isPublic() || "user".equals(pe.getModule())) {
			return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
		} 
		if (pe.isPrivate()) {
			return ImageRepository.getImage(ImageRepository.PE_PRIVATE);
		} if (pe.isLocal()) {
			return ImageRepository.getImage(ImageRepository.PE_LOCAL);
		}
		return ImageRepository.getImage(ImageRepository.PE_PROTECTED);
	}

	@Override
	public String getText(Object element) {
		if(element instanceof SearchPredicateElement){
			SearchPredicateElement pe = ((SearchPredicateElement)element);
			String label = pe.getLabel();
			int count = pe.numberOfOccurences();
//			int count = this.prologSearchResultPage.getDisplayedMatchCount(element);
			String plural = (count==1)?"":"es";
			return label+ " (" + count +" match"+plural+")";
		} else if (element instanceof SearchFileTreeElement){
			SearchFileTreeElement fileTreeElement = ((SearchFileTreeElement) element);
			String label = fileTreeElement.getLabel();
			int count = fileTreeElement.getNumberOfChildren();
			String plural = ((count==1) ? "" : "es");
			return label + " (" + count + " match" + plural + ")";
		} else if(element instanceof PrologTreeElement){
			return ((PrologTreeElement)element).getLabel();
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