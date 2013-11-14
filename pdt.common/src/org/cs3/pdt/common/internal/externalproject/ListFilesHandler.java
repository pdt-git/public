package org.cs3.pdt.common.internal.externalproject;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.handlers.HandlerUtil;

public class ListFilesHandler extends AbstractHandler {
	
	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection currentSelectionChecked = HandlerUtil.getCurrentSelectionChecked(event);
		if (!(currentSelectionChecked instanceof IStructuredSelection)) {
			return null;
		}
		Object element = ((IStructuredSelection) currentSelectionChecked).getFirstElement();
		if (element instanceof IFile) {
			IFile file = (IFile) element;
			IContainer parent = file.getParent();
			if (parent instanceof IFolder) {
				new FolderKeeper().clearAndListAll((IFolder) parent);
			}
		} else if (element instanceof IFolder) {
			new FolderKeeper().clearAndListAll((IFolder) element);
		}
		
		return null;
	}
}
