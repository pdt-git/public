package pdt.pefgraph.internal.views;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.cs3.svf.hyperbolic.HyperbolicGraphView;
import org.cs3.svf.hyperbolic.model.DefaultEdge;
import org.cs3.svf.hyperbolic.model.DefaultGraph;
import org.cs3.svf.hyperbolic.model.DefaultNode;
import org.cs3.svf.hyperbolic.model.Edge;
import org.cs3.svf.hyperbolic.model.Graph;
import org.cs3.svf.hyperbolic.model.Node;
import org.cs3.svf.hyperbolic.model.RandomGraphModelCreator;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;

import pdt.pefgraph.internal.Activator;
import pdt.pefgraph.internal.ImageRepository;

import sun.rmi.server.Activation.DefaultExecPolicy;

public class PEFGraphView extends HyperbolicGraphView {

	private PrologInterface pif;

	private Action selectPifAction;

	private Action refreshAction;

	// private Action action2;
	private final static String HOOK_ID = "PefGraphViewHook";

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	public void createPartControl(Composite parent) {
		super.createPartControl(parent);
		makeActions();
		hookContextMenu();
		contributeToActionBars();
		RandomGraphModelCreator modelCreator = new RandomGraphModelCreator();
		setModel(modelCreator.createGraph(40));
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				PEFGraphView.this.fillContextMenu(manager);
			}
		});

	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(selectPifAction);
		manager.add(refreshAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(selectPifAction);
		manager.add(refreshAction);

		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(selectPifAction);
		manager.add(refreshAction);
		// manager.add(action2);
	}

	private void makeActions() {
		selectPifAction = new SelectPifAction() {

			@Override
			protected PrologInterface getPrologInterface() {
				return pif;
			}

			@Override
			protected void setPrologInterface(PrologInterface prologInterface) {
				if (pif != null) {
					detachFromPif();
					clearGraph();
				}
				pif = prologInterface;
				if (pif != null) {
					attachToPif();
				}

				createGraph();

			}

		};
		refreshAction = new Action() {
			@Override
			public void run() {
				if (pif != null && pif.isUp()) {
					clearGraph();
					createGraph();
				}
			}
		};
		refreshAction.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.RESTART));
	}

	LifeCycleHook2 hook = new LifeCycleHook2() {

		public void onError(PrologInterface pif) {
			clearGraph();

		}

		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
			setupPif();
			createGraph();
		}

		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {
			clearGraph();
		}

		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {
			;
		}

	};

	private void clearGraph() {
		setModel(new DefaultGraph());

	}

	private void attachToPif() {

		pif.addLifeCycleHook(hook, HOOK_ID, new String[] {});
		if (pif.isUp()) {
			try {
				hook.afterInit(pif);
			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			}
		}
		;

	}

	protected void detachFromPif() {
		((PrologInterface2) pif).removeLifeCycleHook(hook, HOOK_ID);

	}

	private static class _DefaultEdge extends DefaultEdge {
		public _DefaultEdge(Graph graph, Node fromNode, Node toNode,
				String caption) {
			super(graph, fromNode, toNode);
			this.caption = caption;
		}

		@Override
		public String getCaption() {

			return caption;
		}

		String caption;
	}

	private void createGraph() {
		Graph graph = getModel();
		HashMap nodes = new HashMap();
		List edges = new Vector();
		PrologSession s = null;
		try {
			s = pif.getSession();
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
		try {
			List list = s
					.queryAll("pef_base:pef_edge(From,FromType,Label,To,ToType)");

			for (Iterator it = list.iterator(); it.hasNext();) {
				Map m = (Map) it.next();
				String from = (String) m.get("From");
				String fromType = (String) m.get("FromType");
				String fromLabel = from + ":" + fromType;
				String label = (String) m.get("Label");
				String to = (String) m.get("To");
				String toType = (String) m.get("ToType");
				String toLabel = to + ":" + toType;
				Node fromNode = (Node) nodes.get(fromLabel);
				if (fromNode == null) {
					fromNode = new DefaultNode(graph, fromLabel);
					nodes.put(fromLabel, fromNode);
				}
				Node toNode = (Node) nodes.get(toLabel);
				if (toNode == null) {
					toNode = new DefaultNode(graph, toLabel);
					nodes.put(toLabel, toNode);
				}
				_DefaultEdge edge = new _DefaultEdge(graph, fromNode, toNode,
						label);
				edges.add(edge);
			}
			graph.addElements(nodes.values(), edges);

		} catch (PrologException e) {
			Debug.rethrow(e);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

	private void setupPif() throws PrologInterfaceException {
		PrologLibraryManager manager = PrologRuntimePlugin.getDefault()
				.getLibraryManager();
		PrologSession s = PEFGraphView.this.pif.getSession();
		try {
			PLUtil.configureFileSearchPath(manager, s,
					new String[] { PrologRuntime.LIB_ATTIC });
			s.queryOnce("use_module(library('spike/pef_base'))");
			s.queryOnce("use_module(library('spike/pef_api'))");
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

}