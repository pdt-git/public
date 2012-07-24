/* $LICENSE_MSG$(ab) */

package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.structureElements.PrologMatch;
import org.cs3.pdt.internal.structureElements.PrologSearchTreeElement;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ITreeContentProvider;


public class PrologSearchTreeContentProvider extends PrologSearchContentProvider implements ITreeContentProvider {
	PrologSearchTreeContentProvider(PrologSearchResultPage page) {
		super(page);
	}

	@Override
	protected synchronized void initialize(PrologSearchResult result) {
		super.initialize(result);
	}

	@Override
	public Object getParent(Object child) {
		if (child==null || getSearchResult() == null){
			return null;
		} else if (child instanceof PrologSearchTreeElement){
			return ((PrologSearchTreeElement) child).getParent();
		} else if (child instanceof PrologMatch){
			return ((PrologMatch) child).getElement();
		} else {
			return null;
		}
	}

	

	@Override
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	@Override
	public Object[] getChildren(Object parentElement) {
		if (parentElement==null||getSearchResult()==null){
			return new Object[0];
		} else if (parentElement instanceof PrologSearchResult){
			return ((PrologSearchResult) parentElement).getChildren();
		} else if (parentElement instanceof PrologSearchTreeElement) {
			return ((PrologSearchTreeElement) parentElement).getChildren();
		} else {
			return new Object[0];
		}
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element==null || getSearchResult() == null) {
			return false;
		} else if (element instanceof PrologSearchTreeElement) {
			return ((PrologSearchTreeElement)element).hasChildren();
		} else {
			return (element instanceof IFile) || (element instanceof PrologSearchResult);
		}
	}

	@Override
	public void clear() {
		initialize(getSearchResult());
		getPage().getViewer().refresh();
	}

	@Override
	public void elementsChanged(Object[] updatedElements) {
	}
}
