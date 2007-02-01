package org.cs3.jtransformer.internal.actions;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.jtransformer.internal.views.PEFNavigatorView;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.IAction;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

public class GotoPEFNavigatorAction extends ConsoleSelectionAction {
	
	

	public void run(IAction action) {
		int intId = getPefId();

		try {
			PlatformUI.getWorkbench().getActiveWorkbenchWindow().
			getActivePage().showView(PEFNavigatorView.ID);
			
			PEFNavigatorView.addId(
					""+intId,
					PrologConsolePlugin.getDefault().getPrologConsoleService().
					      getActivePrologConsole().getPrologInterface());
	
		} catch (PartInitException e) {
			e.printStackTrace();
		} catch (PrologInterfaceException e)
		{
			UIUtils.logAndDisplayError(
					JTransformerPlugin.getDefault().getErrorMessageProvider(),
					UIUtils.getDisplay().getActiveShell(),
					JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
					JTransformer.ERR_CONTEXT_ACTION_FAILED,
					e
					);
		} 
	

	}

}
