/* $LICENSE_MSG$ */

package org.cs3.pdt.internal.actions;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
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
		List<IFile> filesToConsult = new ArrayList<IFile>();
		for (Iterator<?> iter = selection.iterator(); iter.hasNext();) {
			Object obj = iter.next();
			
			if (obj instanceof IFile) {
				filesToConsult.add((IFile) obj);
			}
		}
		
		if (filesToConsult.size() == 1) {
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().consultFile(filesToConsult.get(0));
//			new ConsultAction().consultWorkspaceFile(filesToConsult.get(0));
		} else {
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().consultFiles(filesToConsult);
//			new ConsultAction().consultWorkspaceFiles(filesToConsult);
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

