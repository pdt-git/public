package org.cs3.jtransformer.internal.actions;

import org.cs3.jtransformer.internal.views.PEFNavigatorView;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public class GotoPEFNavigatorAction extends ConsoleSelectionAction {
	
	

	public void run() {
		int intId = getPefId();

		try {
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().
			getActivePage().showView(PEFNavigatorView.ID);
			PEFNavigatorView.addId(""+intId);
		} catch (PartInitException e) {
			e.printStackTrace();
		}
	}

}
