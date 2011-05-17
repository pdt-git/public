package org.cs3.pdt.internal.actions;

import org.cs3.pdt.PDTPlugin;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;

public class OpenConsoleAction implements IWorkbenchWindowActionDelegate {

	@Override
	public void dispose() {

	}

	@Override
	public void init(IWorkbenchWindow window) {

	}

	@Override
	public void run(IAction action) {
		final IWorkbenchPage activePage = PDTPlugin.getActivePage();
		if (activePage == null) {
			return;
		}
		try {
			activePage.showView("org.cs3.pdt.console.internal.views.PrologConsoleView");
		} catch (PartInitException e) {
//			UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(), UIUtils.getDisplay().getActiveShell(), 
//					PDTCore.ERR_UNKNOWN, PDTCore.CX_UNKNOWN, e);
		}
	}

	@Override
	public void selectionChanged(IAction action, ISelection selection) {

	}

}
