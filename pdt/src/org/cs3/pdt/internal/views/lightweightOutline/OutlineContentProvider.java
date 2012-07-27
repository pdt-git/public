package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.PrologTreeElement;
import org.eclipse.jdt.ui.IWorkingCopyProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

class OutlineContentProvider implements ITreeContentProvider, IWorkingCopyProvider  {


	/**
	 * Creates a new Outline content provider.
	 *
	 * @param showInheritedMembers <code>true</code> iff inherited members are shown
	 */
	OutlineContentProvider() {

	}


	/**
	 * {@inheritDoc}
	 */
	@Override
	public Object[] getChildren(Object element) {
		if(element instanceof PrologSourceFileModel) {
			return ((PrologSourceFileModel)element).getElements();
		}	
		if(element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).getChildren();
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

	}

	/**
	 * {@inheritDoc}
	 */
	@Override
	public void dispose() {

	}


	@Override
	public boolean providesWorkingCopies() {
		return false;
	}


	@Override
	public Object[] getElements(Object element) {
		if(element instanceof PrologSourceFileModel) {
			if(((PrologSourceFileModel)element).hasChildren())
				return ((PrologSourceFileModel)element).getElements();
		}	
		if(element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).getChildren();
		}
		return new Object[0];
	}


	@Override
	public Object getParent(Object element) {
		return null;
	}


	@Override
	public boolean hasChildren(Object element) {
		if(element instanceof PrologTreeElement)
			return ((PrologTreeElement)element).hasChildren();
		return false;
	}
}