package pdt.pefgraph.internal.views;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.prolog.IPrologEventDispatcher;
import org.cs3.pl.prolog.LifeCycleHook2;
import org.cs3.pl.prolog.PLUtil;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterface2;
import org.cs3.pl.prolog.PrologInterfaceEvent;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologInterfaceListener;
import org.cs3.pl.prolog.PrologLibraryManager;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.mylyn.zest.core.viewers.GraphViewer;
import org.eclipse.mylyn.zest.core.viewers.IGraphContentProvider;
import org.eclipse.mylyn.zest.core.widgets.GraphConnection;
import org.eclipse.swt.widgets.Display;

public class PEFGraphContentProvider implements IGraphContentProvider,
		PrologInterfaceListener {
	private static class PseudoEdge{
		public String nodeId;
	}
	private PrologInterface pif;
	private HashSet<PEFEdge> edges = null;
	private HashMap<String, PEFNode> nodes = null;
	private HashMap<String,Vector<PEFEdge>> outgoingEdges = null;
	private HashMap<String,Vector<PEFEdge>> incomingEdges = null;
	private HashMap<String,PseudoEdge> pseudoEdges = null;
	private StructuredViewer viewer;

	
	public Vector<PEFEdge> getIncomingEdges(String nodeId){
		if(incomingEdges==null){
			incomingEdges=new HashMap<String, Vector<PEFEdge>>();
		}
		Vector<PEFEdge> edges = incomingEdges.get(nodeId);
		if(edges==null){
			edges = new Vector<PEFEdge>();
			incomingEdges.put(nodeId, edges);
		}
		return edges;
	}
	
	public Vector<PEFEdge> getOutgoingEdges(String nodeId){
		if(outgoingEdges==null){
			outgoingEdges=new HashMap<String, Vector<PEFEdge>>();
		}
		Vector<PEFEdge> edges = incomingEdges.get(nodeId);
		if(edges==null){
			edges = new Vector<PEFEdge>();
			outgoingEdges.put(nodeId, edges);
		}
		return edges;
	}
	
	public Object getDestination(Object rel) {
		if (rel instanceof PEFEdge){
			PEFEdge edge = (PEFEdge) rel;
			return getNode(edge.to);	
		}
		return null;
	}

	public PEFNode getNode(String id) {
		Debug.debug("node " + id + " requested");
		
		if (getNodes().containsKey(id)) {
			return getNodes().get(id);
		}
		PEFNode result = fetchNode(id);
		getNodes().put(id, result);
		return result;

	}

	private void fetchNodes() {
		nodes = new HashMap<String, PEFNode>();
		Debug.debug("fetching nodes");
		if (pif == null || !pif.isUp()) {
			return;
		}

		PrologSession s = null;
		try {
			s = pif.getSession();
			List l = s.queryAll("pef_graph_node(Id, Type,Labels)");
			for (Object o : l) {

				Map m = (Map) o;
				String id = (String) m.get("Id");
				
				String type = (String) m.get("Type");
				List list = (List) m.get("Labels");
				String[] labels = (String[]) (list).toArray(new String[(list)
						.size()]);
				PEFNode result = new PEFNode(id, type, labels, pif);
				nodes.put(id, result);
				if(getIncomingEdges(id).isEmpty()&&getOutgoingEdges(id).isEmpty()){
					addPseudoEdge(id);
				}
			}

		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}

	}

	private PEFNode fetchNode(String id) {
		Debug.debug("fetching node " + id);
		if (pif == null || !pif.isUp()) {
			return null;
		}

		PrologSession s = null;
		PEFNode result=null;
		try {
			s = pif.getSession();
			Map m = s.queryOnce("pef_graph_node(" + id + ", Type,Labels)");
			if (m == null) {
				return null;
			}
			String type = (String) m.get("Type");
			List labels0 = (List) m.get("Labels");

			String[] labels = (String[]) labels0.toArray(new String[labels0
					.size()]);

			
			result = new PEFNode(id, type, labels,
					pif);
			if(getIncomingEdges(id).isEmpty()&&getOutgoingEdges(id).isEmpty()){
				addPseudoEdge(id);
			}
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
		return result;
	}

	public Object[] getElements(Object input) {
		if (pif == null || !pif.isUp()) {
			return new Object[0];
		}
		Vector<Object> hector = new Vector<Object>();
		hector.addAll(getEdges());
		hector.addAll(getPseudoEdges().values());
		return hector.toArray(new Object[hector.size()]);

	}

	private HashSet<PEFEdge> getEdges() {
		Debug.debug("edges requested");
		if (edges == null) {
			fetchEdges();
		}
		return edges;
	}

	private void fetchEdges() {
		Debug.debug("fetching edges");
		edges = new HashSet<PEFEdge>();
		PrologSession s = null;
		try {
			s = pif.getSession();
			List l = s.queryAll("pef_graph_edge(From,Label,To)");

			int i = 0;
			for (Object object : l) {
				Map m = (Map) object;

				String from = (String) m.get("From");
				String label = (String) m.get("Label");
				String to = (String) m.get("To");
				PEFEdge edge = new PEFEdge(from, label, to);
				edges.add(edge);
				removePseudoEdge(from);
				removePseudoEdge(to);
				getIncomingEdges(to).add(edge);
				getOutgoingEdges(from).add(edge);
			}
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

	private HashMap<String, PseudoEdge> getPseudoEdges() {
		if(pseudoEdges==null){			
			pseudoEdges=new HashMap<String, PseudoEdge>();
			getNodes();
		}
		return pseudoEdges;
	}

	private void addPseudoEdge(String nodeId){
		HashMap<String, PseudoEdge> map = getPseudoEdges();
		if(!map.containsKey(nodeId)){
			PseudoEdge e = new PseudoEdge();
			e.nodeId=nodeId;
			map.put(nodeId, e);
		}
	}
	
	private void removePseudoEdge(String nodeId){
		HashMap<String, PseudoEdge> map = getPseudoEdges();
		if(map.containsKey(nodeId)){			
			map.remove(nodeId);
		}
	}
	
	public Object getSource(Object rel) {
		if(rel instanceof PEFEdge){
			PEFEdge edge = (PEFEdge) rel;
			return getNode(edge.from);	
		} else if(rel instanceof PseudoEdge){
			return getNode(((PseudoEdge)rel).nodeId);
		}
		else return null;
	}

	public double getWeight(Object connection) {
		return -1;
	}

	public void dispose() {
		if (pif != null) {
			detachFromPif();
		}
	}

	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

		this.viewer = (StructuredViewer) viewer;
		if (oldInput == newInput) {
			return;
		}
		if (pif != null) {
			detachFromPif();
		}
		reset();
		if (newInput != null && newInput instanceof PrologInterface) {
			attachToPif((PrologInterface) newInput);
		}
	}

	public void reset() {
		edges = null;
		nodes = null;
		outgoingEdges = null;
		incomingEdges = null;
		pseudoEdges = null;
		viewer.refresh();

		GraphViewer gv = (GraphViewer) viewer;
		gv.applyLayout();
	}

	// private Action action2;
	private final static String HOOK_ID = "PefGraphViewHook";

	private void attachToPif(PrologInterface pif) {
		this.pif = pif;
		try {
			pif.addLifeCycleHook(hook, HOOK_ID, new String[] {});
			if (pif.isUp()) {

				hook.onInit(pif, pif.getSession());

			}
			IPrologEventDispatcher d = PrologRuntimePlugin.getDefault()
					.getPrologEventDispatcher(pif);
			d.addPrologInterfaceListener("pef_graph", this);
			d.addPrologInterfaceListener("pef_node(_)", this);
			d.addPrologInterfaceListener("pef_edge(_,_,_)", this);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}
	}

	protected void detachFromPif() {
		((PrologInterface2) pif).removeLifeCycleHook(hook, HOOK_ID);
		try {
			IPrologEventDispatcher d = PrologRuntimePlugin.getDefault()
					.getPrologEventDispatcher(pif);
			d.removePrologInterfaceListener("pef_graph", this);
			d.removePrologInterfaceListener("pef_node(_)", this);
			d.removePrologInterfaceListener("pef_edge(_,_,_)", this);
		} catch (PrologInterfaceException e) {
			Debug.rethrow(e);
		}

	}

	private void setupPif(PrologSession s) throws PrologInterfaceException {
		PrologLibraryManager manager = PrologRuntimePlugin.getDefault()
				.getLibraryManager();

		try {
			PLUtil.configureFileSearchPath(manager, s,
					new String[] { PrologRuntime.LIB_PDT });
			s.queryOnce("use_module(library('facade/pdt_pef_graph'))");

		} finally {
			if (s != null) {
				s.dispose();
			}
		}
	}

	LifeCycleHook2 hook = new LifeCycleHook2() {

		public void onError(PrologInterface pif) {

		}

		public void afterInit(PrologInterface pif)
				throws PrologInterfaceException {
			reset();

		}

		public void beforeShutdown(PrologInterface pif, PrologSession session)
				throws PrologInterfaceException {

		}

		public void onInit(PrologInterface pif, PrologSession initSession)
				throws PrologInterfaceException {

			setupPif(initSession);

		}

		@Override
		public void setData(Object data) {
			// TODO Auto-generated method stub
			
		}

	};

	public void update(PrologInterfaceEvent e) {
		String event = e.getEvent();
		CTerm subject = PLUtil.createCTerm(e.getSubject());

		String functor = subject.getFunctorValue();
		String[] args = new String[subject.getArity()];

		for (int i = 0; i < args.length; i++) {
			args[i] = ((CCompound) subject).getArgument(i).getFunctorValue();
		}
		if (event.equals("added")) {
			if (functor.equals("pef_node")) {
				addNode(args[0]);
			} else if (functor.equals("pef_edge")) {
				addEdge(args[0], args[1], args[2]);
			}
		} else if (event.equals("removed")) {
			if (functor.equals("pef_node")) {
				removeNode(args[0]);
			} else if (functor.equals("pef_edge")) {
				removeEdge(args[0], args[1], args[2]);
			}
		} else if (event.equals("refresh")) {
			fireRefresh();
		}
	}

	private void removeEdge(String from, String label, String to) {
		if (getNode(from) == null && getNode(to) == null) {
			PEFEdge edge = new PEFEdge(from, label, to);
			if (edges.remove(edge)) {
				;// fireEdgeRemoved(edge);
			}
		}
	}

	private void removeNode(String id) {		
		PEFNode node = getNodes().remove(id);
		removePseudoEdge(id);
		if (node != null) {
			// fireNodeRemoved(node);
		}
	}

	private void addNode(String id) {		
		PEFNode node = getNodes().get(id);
		if (node == null) {
			node = fetchNode(id);
			getNodes().put(id, node);
			// fireNodeAdded(node);
		}

	}

	private HashMap<String, PEFNode> getNodes() {
		if(nodes==null){
			fetchNodes();
		}
		return nodes;
	}

	private void addEdge(String from, String label, String to) {
		if (getNode(from) != null || getNode(to) != null) {
			PEFEdge edge = new PEFEdge(from, label, to);
			if (getEdges().add(edge)) {
				;
			}
		}
	}

	private void fireRefresh() {
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			display.asyncExec(new Runnable() {
				public void run() {
					fireRefresh();

				}
			});
			return;
		}
		viewer.refresh();
		// GraphViewer gv = (GraphViewer) viewer;
		// gv.applyLayout();

	}

}
