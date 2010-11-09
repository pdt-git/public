package org.cs3.pdt.internal.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public class OpenConsoleAction implements IWorkbenchWindowActionDelegate {

	@Override
	public void dispose() {

	}

	@Override
	public void init(IWorkbenchWindow window) {

	}

	@Override
	public void run(IAction action) {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
				.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return;
		}

		final IWorkbenchPage activePage = activeWorkbenchWindow.getActivePage();
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
