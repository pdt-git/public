/*
 * Created on 23.08.2004
 *
 */
package org.cs3.pl.search;

import org.cs3.pl.PrologElement;
import org.cs3.pl.prolog.PrologElementData;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.search.ui.ISearchQuery;
import org.eclipse.search.ui.text.AbstractTextSearchResult;
import org.eclipse.search.ui.text.IEditorMatchAdapter;
import org.eclipse.search.ui.text.IFileMatchAdapter;
import org.eclipse.search.ui.text.Match;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.model.IWorkbenchAdapter;
import org.eclipse.ui.part.FileEditorInput;

/**
 * @author rho
 *
 */
public class PrologSearchResult extends AbstractTextSearchResult implements IEditorMatchAdapter, IFileMatchAdapter  {

	private PrologSearchQuery query;
	private PrologElementData data;
	private final Match[] EMPTY_ARR= new Match[0];
	/**
	 * @param query
	 * @param queryString
	 */
	public PrologSearchResult(PrologSearchQuery query, PrologElementData data) {
		this.query = query;
		this.data = data;
	}

	public IEditorMatchAdapter getEditorMatchAdapter() {
		return this;
	}

	public IFileMatchAdapter getFileMatchAdapter() {
		return this;
	}

	public String getLabel() {
		return "Prolog Search: " + data.getSignature();
	}

	public String getTooltip() {
		return "Prolog Search: " + data.getSignature();
	}

	public ImageDescriptor getImageDescriptor() {
		PrologElement element = new PrologElement(data);
		return ((IWorkbenchAdapter)element.getAdapter(IWorkbenchAdapter.class)).getImageDescriptor(null);
	}

	public ISearchQuery getQuery() {
		return query;
	}

	public boolean isShownInEditor(Match match, IEditorPart editor) {
		IEditorInput ei= editor.getEditorInput();
		if (ei instanceof IFileEditorInput) {
			FileEditorInput fi= (FileEditorInput) ei;
			return match.getElement().equals(fi.getFile());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.search.ui.text.IEditorMatchAdapter#computeContainedMatches(org.eclipse.search.ui.text.AbstractTextSearchResult, org.eclipse.ui.IEditorPart)
	 */
	public Match[] computeContainedMatches(AbstractTextSearchResult result, IEditorPart editor) {
		IEditorInput ei= editor.getEditorInput();
		if (ei instanceof IFileEditorInput) {
			FileEditorInput fi= (FileEditorInput) ei;
			return result.getMatches(fi.getFile());
		}
		return EMPTY_ARR;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.search.ui.text.IFileMatchAdapter#computeContainedMatches(org.eclipse.search.ui.text.AbstractTextSearchResult, org.eclipse.core.resources.IFile)
	 */
	public Match[] computeContainedMatches(AbstractTextSearchResult result, IFile file) {
		return result.getMatches(file);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.search.ui.text.IFileMatchAdapter#getFile(java.lang.Object)
	 */
	public IFile getFile(Object element) {
		if (element instanceof IFile)
			return (IFile)element;
		return null;
	}

}
