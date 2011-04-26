/**
 * 
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.swt.graphics.Image;

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
		} else 	if (element instanceof PrologMatch) {
			return ImageRepository.getImage(ImageRepository.MATCH);
		} else if(element instanceof PredicateElement){
			PredicateElement pe = (PredicateElement) element;
			if("this_pred_ref".equals(pe.getType())){
				return ImageRepository.getImage(ImageRepository.VERIFIED_MATCH);
			}
			if("unresolved_pred_ref".equals(pe.getType())){
				return ImageRepository.getImage(ImageRepository.UNRESOLVED_PRED_MATCH);
			}
			return ImageRepository.getImage(ImageRepository.POTENTIAL_MATCH);
		} 
		return null;
	}

	@Override
	public String getText(Object element) {
		if(element instanceof PredicateElement){
			
			PredicateElement pe = ((PredicateElement)element);
			String label = pe.getLabel();
			String module = pe.getType();
			if (module == null || module.isEmpty())
				module = "user";
			String fullLabel = module + ":" + label;
			int count = this.prologSearchResultPage.getDisplayedMatchCount(element);
			String plural = (count==1)?"":"es";
			return fullLabel+ " (" + count +" match"+plural+")";
		}
		if(element instanceof IFile){
			return ((IFile)element).getFullPath().toString();
		}
		if(element instanceof PrologMatch) {
			return Integer.toString(((PrologMatch)element).getLine());
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