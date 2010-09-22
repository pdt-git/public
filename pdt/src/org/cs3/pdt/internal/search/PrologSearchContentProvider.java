package org.cs3.pdt.internal.search;

/*copied and adapted from the jdt source*/

import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.Viewer;

public abstract class PrologSearchContentProvider implements IStructuredContentProvider {
	protected final Object[] EMPTY_ARR= new Object[0];
	
	private PrologSearchResult fResult;
	private PrologSearchResultPage fPage;

	PrologSearchContentProvider(PrologSearchResultPage page) {
		fPage= page;
	}
	
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		initialize((PrologSearchResult) newInput);
		
	}
	
	protected void initialize(PrologSearchResult result) {
		fResult= result;
	}
	
	public abstract void elementsChanged(Object[] updatedElements);
	public abstract void clear();

	@Override
	public void dispose() {
		// nothing to do
	}

	PrologSearchResultPage getPage() {
		return fPage;
	}
	
	PrologSearchResult getSearchResult() {
		return fResult;
	}

}
