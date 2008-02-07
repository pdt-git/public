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
		boolean visible = !isVisible(node);
		setVisible(node, visible);
		if (visible = true) {
			String viewId = "pdt.pefgraph.views.PEFGraphView";
			PEFGraphView view = (PEFGraphView) showView(viewId);
			view.getViewer().setInput(node.getPrologInterface());
		}
		action.setChecked(visible);
	}

	private static IViewPart showView(String viewId) {
		final IWorkbenchWindow activeWorkbenchWindow = PlatformUI
				.getWorkbench().getActiveWorkbenchWindow();
		if (activeWorkbenchWindow == null) {
			return null;
		}

		final IWorkbenchPage activePage = activeWorkbenchWindow
				.getActivePage();
		if (activePage == null) {
			return null;
		}

		try {
			
			return activePage
					.showView(viewId);
			
		} catch (PartInitException e) {
			UIUtils.logAndDisplayError(Activator.getDefault()
					.getErrorMessageProvider(), UIUtils.getDisplay()
					.getActiveShell(), PEFGraph.ERR_UNKNOWN,
					PEFGraph.CX_SHOWING_PART, e);

		}
		return null;
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

		action.setChecked(isVisible(node));
	}

	private boolean isVisible(PEFHandle node) {
		PrologInterface pif = node.getPrologInterface();
		if (pif == null || !pif.isUp()) {
			return false;
		}
		PrologSession s = null;
		try {
			s = pif.getSession();
			return null != s.queryOnce("pef_graph_node(" + node.getId()
					+ ",_,_)");

		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}

		}
		return false;

	}

	private void setVisible(PEFHandle node, boolean visible) {
		PrologInterface pif = node.getPrologInterface();
		if (pif == null || !pif.isUp()) {
			return;
		}
		PrologSession s = null;
		try {
			s = pif.getSession();
			s.queryOnce("pef_graph_set_visible(" + node.getId() + "," + visible
					+ "),pef_graph_refresh");

		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

}
