package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.structureElements.PrologOutlineTreeElement;
import org.cs3.pdt.internal.structureElements.PrologTreeElement;
import org.eclipse.jdt.ui.IWorkingCopyProvider;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.Viewer;

class OutlineContentProvider implements ITreeContentProvider, IWorkingCopyProvider  {


	private static final Object[] EMPTY_OBJECT_ARRAY = new Object[0];


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
		if (element instanceof PrologSourceFileModel) {
			if (((PrologSourceFileModel)element).hasChildren()) {
				return ((PrologSourceFileModel)element).getElements();
			}
		} else if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).getChildren();
		}
		return EMPTY_OBJECT_ARRAY;
	}


	@Override
	public Object getParent(Object element) {
		if (element instanceof PrologOutlineTreeElement) {
			return ((PrologOutlineTreeElement) element).getParent();
		} else {
			return EMPTY_OBJECT_ARRAY;
		}
	}


	@Override
	public boolean hasChildren(Object element) {
		if (element instanceof PrologTreeElement) {
			return ((PrologTreeElement)element).hasChildren();
		} else {
			return false;
		}
	}
}