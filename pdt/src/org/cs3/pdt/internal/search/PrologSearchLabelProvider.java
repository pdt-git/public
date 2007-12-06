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

	public Image getImage(Object element) {
		//TODO: correct image
		if(!(element instanceof PredicateElement)){
			return null;
		}
		PredicateElement pe = (PredicateElement) element;
		if("this_pred_ref".equals(pe.type)){
			return ImageRepository.getImage(ImageRepository.VERIFIED_MATCH);
		}
		if("unresolved_pred_ref".equals(pe.type)){
			return ImageRepository.getImage(ImageRepository.UNRESOLVED_PRED_MATCH);
		}
		return ImageRepository.getImage(ImageRepository.POTENTIAL_MATCH);
	}

	public String getText(Object element) {
		if(element instanceof PredicateElement){
			
			PredicateElement pe = ((PredicateElement)element);
			String label = pe.label;
			if(! "this_pred_ref".equals(pe.type)){
				label += " ("+pe.type+")";
			}
			int count = this.prologSearchResultPage.getDisplayedMatchCount(element);
			String plural = (count==1)?"":"es";
			return label+ " (" + count +" match"+plural+")";
		}
		if(element instanceof IFile){
			return ((IFile)element).getFullPath().toString();
		}
		return "no label";
	}

	public void addListener(ILabelProviderListener listener) {
	}

	public void dispose() {
	}

	public boolean isLabelProperty(Object element, String property) {
		return false;
	}

	public void removeListener(ILabelProviderListener listener) {
	}
}