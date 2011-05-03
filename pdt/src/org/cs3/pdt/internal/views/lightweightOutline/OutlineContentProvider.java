package org.cs3.pdt.internal.views.lightweightOutline;

import org.cs3.pdt.internal.editors.PrologSourceFileModel;
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
	public Object[] getChildren(Object element) {
		if(element instanceof PrologSourceFileModel){
			return ((PrologSourceFileModel)element).getPredicates().toArray();
		}
		return null;
	}

	/**
	 * {@inheritDoc}
	 */
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

	}

	/**
	 * {@inheritDoc}
	 */
	public void dispose() {

	}


	@Override
	public boolean providesWorkingCopies() {
		return false;
	}


	@Override
	public Object[] getElements(Object element) {
		if(element instanceof PrologSourceFileModel){
			return ((PrologSourceFileModel)element).getPredicates().toArray();
		}
		return null;
	}


	@Override
	public Object getParent(Object element) {
		return null;
	}


	@Override
	public boolean hasChildren(Object element) {
		return false;
	}
}