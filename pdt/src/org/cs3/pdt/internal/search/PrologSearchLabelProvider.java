/**
 * 
 */
package org.cs3.pdt.internal.search;

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
		return PrologSearchResultPage.IMAGE;
	}

	public String getText(Object element) {
		if(element instanceof PredicateElement){
			int count = this.prologSearchResultPage.getDisplayedMatchCount(element);
			String plural = (count==1)?"":"es";
			return ((PredicateElement)element).label+ " (" + count +" match"+plural+")";
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