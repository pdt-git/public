/*******************************************************************************
 * Copyright (c) 2000, 2006 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.structureElements.PDTMatch;
import org.cs3.pdt.internal.structureElements.PDTSearchTreeElement;
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
		} else if (child instanceof PDTSearchTreeElement){
			return ((PDTSearchTreeElement) child).getParent();
		} else if (child instanceof PDTMatch){
			return ((PDTMatch) child).getElement();
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
		} else if (parentElement instanceof PDTSearchTreeElement) {
			return ((PDTSearchTreeElement) parentElement).getChildren();
		} else {
			return new Object[0];
		}
	}

	@Override
	public boolean hasChildren(Object element) {
		if(element==null || getSearchResult() == null) {
			return false;
		} else if (element instanceof PDTSearchTreeElement) {
			return ((PDTSearchTreeElement)element).hasChildren();
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
