package org.cs3.pdt.internal.actions;

import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;

public class ReloadAction implements IObjectActionDelegate  {

	private IStructuredSelection selection;

	public ReloadAction() {
	}

	@Override
	public void run(IAction action) {
		for (Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			Object obj = iter.next();
			
			if (obj instanceof IFile) {
				IFile f = (IFile) obj;
				new ConsultAction().consultWorkspaceFile(f);
//				PLMarkerUtils.updateFileMarkers( (IFile) obj);
			}

		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {
		if (selection instanceof IStructuredSelection) {
			this.selection = (IStructuredSelection) selection;
		}

	}

	@Override
	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
	}

}
