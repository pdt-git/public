package org.cs3.jtransformer.internal.actions;

import org.cs3.jtransformer.util.JTUtils;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public class OpenPEFNavigatorAction implements IWorkbenchWindowActionDelegate {

	public void dispose() {
	}

	public void init(IWorkbenchWindow window) {
	}

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
			activePage
					.showView("org.cs3.jtransformer.internal.navigator.PEFNavigatorView");
		} catch (PartInitException e) {
			JTUtils.logAndDisplayUnknownError(e);

		}

	}

	public void selectionChanged(IAction action, ISelection selection) {

	}

}
