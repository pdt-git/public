package pdt.pefgraph.internal.views;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
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

import pdt.pefgraph.internal.ImageRepository;

public class PEFGraphView extends HyperbolicGraphView implements
		PrologInterfaceListener {

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
		refreshAction.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.RESTART));
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

	private HashMap nodes;

	private void clearGraph() {
		setModel(new DefaultGraph());

	}

	private void attachToPif() {
		try {
			pif.addLifeCycleHook(hook, HOOK_ID, new String[] {});
			if (pif.isUp()) {

				hook.afterInit(pif);

			}
			IPrologEventDispatcher d = PrologRuntimePlugin.getDefault()
					.getPrologEventDispatcher(pif);
			d.addPrologInterfaceListener("pef_node(_,_)", this);
			d.addPrologInterfaceListener("pef_edge(_,_,_,_)", this);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
	}

	protected void detachFromPif() {
		((PrologInterface2) pif).removeLifeCycleHook(hook, HOOK_ID);
		try {
			IPrologEventDispatcher d = PrologRuntimePlugin.getDefault()
					.getPrologEventDispatcher(pif);
			d.removePrologInterfaceListener("pef_node(_,_)", this);
			d.removePrologInterfaceListener("pef_edge(_,_,_,_)", this);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}

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

		nodes = new HashMap();
		List edges = new Vector();
		PrologSession s = null;
		try {
			s = pif.getSession();
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
		try {
			List list = s.queryAll("pef_graph_node(Id,Type,Labels)");
			for (Iterator iter = list.iterator(); iter.hasNext();) {
				Map m = (Map) iter.next();
				String id = (String) m.get("Id");
				String type = (String) m.get("Type");
				List labels = (List) m.get("Labels");
				String key = id + ":" + type;
				String label = key + " " + Util.splice(labels, ", ");
				Node node = new DefaultNode(graph, label);

				nodes.put(key, node);
			}

			list = s.queryAll("pef_graph_edge(From,FromType,To,ToType,Label)");

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

				Node toNode = (Node) nodes.get(toLabel);
				if (toNode == null) {
					Debug.debug("Debug");
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
					new String[] { PrologRuntime.LIB_PDT });
			s.queryOnce("use_module(library('facade/pdt_pef_graph'))");
			// s.queryOnce("use_module(library('pef/pef_api'))");
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

	public void update(PrologInterfaceEvent e) {
		String event = e.getEvent();
		CCompound subject = (CCompound) PLUtil.createCTerm(e.getSubject());
		String functor = subject.getFunctorValue();
		String[] args = new String[subject.getArity()];
		for (int i = 0; i < args.length; i++) {
			args[i] = subject.getArgument(i).getFunctorValue();
		}
		if (event.equals("added")) {
			if (functor.equals("pef_node")) {
				addNode(args[0], args[1]);
			} else if (functor.equals("pef_edge")) {
				addEdge(args[0], args[1], args[2], args[3]);
			}
		} else if (event.equals("removed")) {
			if (functor.equals("pef_node")) {
				removeNode(args[0], args[1]);
			} else if (functor.equals("pef_edge")) {
				removeEdge(args[0], args[1], args[2], args[3]);
			}
		}
	}

	private void removeEdge(String from, String fromType, String to,
			String toType) {
		Node fromNode = (Node) nodes.get(from + ":" + fromType);
		Node toNode = (Node) nodes.get(to + ":" + toType);
		if(fromNode==null||toNode==null){
			return;
		}
		List<Edge> edges = fromNode.getSourceConnections();
		for (Edge edge : edges) {
			if (edge.getTarget() == toNode) {
				Vector<Edge> l = new Vector<Edge>();
				l.add(edge);
				edge.getGraph().removeElements(new Vector(), l);
			}
		}

	}

	private void removeNode(String id, String type) {
		Node node = (Node) nodes.get(id+ ":" + type);
		if(node!=null){
			Vector<Node> l = new Vector<Node>();
			l.add(node);
			node.getGraph().removeElements(l, new Vector());
		}
	}

	private void addEdge(String from, String fromType, String to, String toType) {
		Node fromNode = (Node) nodes.get(from + ":" + fromType);
		if(fromNode==null){
			throw new IllegalArgumentException("no such node: "+from+":"+fromType);
		}
		Node toNode = (Node) nodes.get(to + ":" + toType);
		if(toNode==null){
			throw new IllegalArgumentException("no such node: "+to+":"+toType);
		}
		Edge edge =  new _DefaultEdge(fromNode.getGraph(), fromNode, toNode,
				"no_label");
		Vector<Edge> l = new Vector<Edge>();
		l.add(edge);
		edge.getGraph().addElements(new Vector(), l);

	}

	private void addNode(String id, String type) {
		Graph graph = getModel();
		Node node = (Node) nodes.get(id+ ":" + type);
		if(node!=null){
			return;
		}
		if(pif==null){
			return;
		}
		PrologSession s = null;
		try{
			s=pif.getSession();
			String query = "pef_graph_node("+id+", "+type+", Labels)";
			Map m = s.queryOnce(query);
			if(m==null){
				throw new RuntimeException("query failed: "+query);
			}
			List labels = (List) m.get("Labels");
			String key = id + ":" + type;
			String label = key + " " + Util.splice(labels, ", ");
			node = new DefaultNode(graph,label);
			Vector<Node> l = new Vector<Node>();
			l.add(node);
			graph.addElements(l, new Vector());
			nodes.put(key,node);
			
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}finally{
			if(s!=null){
				s.dispose();
			}
		}
		
	}

}