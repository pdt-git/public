package pdt.pefgraph.actions;

import java.util.HashMap;

import org.cs3.pdt.core.PEFHandle;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;

import pdt.pefgraph.PEFGraph;
import pdt.pefgraph.internal.Activator;
import pdt.pefgraph.internal.views.PEFGraphView;

public class NodeVisibleAction implements IObjectActionDelegate {

	private HashMap<IAction, PEFHandle> selectedNodes = new HashMap<IAction, PEFHandle>();
	private HashMap<IAction, IWorkbenchPart> activeParts = new HashMap<IAction, IWorkbenchPart>();

	public NodeVisibleAction() {
		// TODO Auto-generated constructor stub
	}

	public void setActivePart(IAction action, IWorkbenchPart targetPart) {
		activeParts.put(action, targetPart);

	}

	public void run(IAction action) {
		PEFHandle node = selectedNodes.get(action);
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
		PEFHandle node = null;
		if (selection == null) {
			node = null;
		} else if (!(selection instanceof IStructuredSelection)) {
			node = null;
		} else if (((IStructuredSelection) selection).size() != 1) {
			node = null;
		} else if (((IStructuredSelection) selection).getFirstElement() instanceof PEFHandle) {
			node = (PEFHandle) ((IStructuredSelection) selection)
					.getFirstElement();
		} else if (((IStructuredSelection) selection).getFirstElement() instanceof IAdaptable) {
			node = (PEFHandle) Platform.getAdapterManager().getAdapter(
					((IStructuredSelection) selection).getFirstElement(),
					PEFHandle.class);
		}
		selectedNodes.put(action, node);
		updateActionState(action, node);
	}

	private void updateActionState(IAction action, PEFHandle node) {
		action.setEnabled(node != null);

		action.setChecked(PEFGraph.isVisible(node));
	}

	

	
}
