package pdt.pefgraph;

import org.cs3.pdt.console.internal.views.PrologConsoleView;
import org.cs3.pdt.core.PEFHandle;
import org.cs3.pdt.core.SimplePEFHandle;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IViewActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.PartInitException;

import pdt.pefgraph.internal.Activator;

public class ShowSelectionInPEFGraphAction implements IViewActionDelegate {

	
	private String nodeId;
	private PrologConsoleView consoleView;

	public ShowSelectionInPEFGraphAction() {
		// TODO Auto-generated constructor stub
	}

	public void init(IViewPart view) {
		consoleView = (PrologConsoleView) view;
		

	}

	public void run(IAction action) {
		PEFHandle node = new SimplePEFHandle(consoleView.getPrologInterface(),nodeId);
		boolean visible = !PEFGraph.isVisible(node);
		PEFGraph.setVisible(node, visible);
		try {
			if (visible = true) {
				PrologInterface prologInterface = node.getPrologInterface();
				PEFGraph.showPEFGraph(prologInterface);
			}
			action.setChecked(visible);
		} catch (PartInitException e) {
			UIUtils.logAndDisplayError(Activator.getDefault()
					.getErrorMessageProvider(), UIUtils.getDisplay()
					.getActiveShell(), PEFGraph.ERR_UNKNOWN,
					PEFGraph.CX_SHOWING_PART, e);

		}
	}


	public void selectionChanged(IAction action, ISelection selection) {
		ITextSelection textSelection = null;
		if (selection instanceof ITextSelection) {
			textSelection = (ITextSelection) selection;
			if (textSelection == null || textSelection.isEmpty()) {
				action.setEnabled(false);
			} else {
				String text = textSelection.getText();
				try {
					Integer.parseInt(text);
					this.nodeId = text;
					action.setEnabled(true);
				} catch (NumberFormatException e) {
					this.nodeId = null;
					action.setEnabled(false);
				}
			}
		}
	}

}
