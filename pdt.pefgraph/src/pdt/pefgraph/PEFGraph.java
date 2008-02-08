package pdt.pefgraph;

import org.cs3.pdt.core.PEFHandle;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.ui.PartInitException;

import pdt.pefgraph.internal.views.PEFGraphView;

public class PEFGraph {

	public final static int ERR_UNKNOWN = -1;
	public final static int CX_SHOWING_PART = -1;

	public static boolean isVisible(PEFHandle node) {
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

	public static void setVisible(PEFHandle node, boolean visible) {
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

	public static void showPEFGraph(PrologInterface prologInterface)
			throws PartInitException {
		String viewId = "pdt.pefgraph.views.PEFGraphView";
		PEFGraphView view = (PEFGraphView) UIUtils.showView(viewId);
		view.getViewer().setInput(prologInterface);
	}
}
