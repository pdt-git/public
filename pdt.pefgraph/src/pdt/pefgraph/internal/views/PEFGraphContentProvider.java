package pdt.pefgraph.internal.views;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.runtime.PrologRuntime;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
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
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.mylyn.zest.core.viewers.IGraphContentProvider;

public class PEFGraphContentProvider implements IGraphContentProvider, PrologInterfaceListener {
	private PrologInterface pif;
	private PEFEdge[] edges = null;
	private HashMap<String, PEFNode> nodes = new HashMap<String, PEFNode>();

	@Override
	public Object getDestination(Object rel) {
		PEFEdge edge = (PEFEdge) rel;
		return getNode(edge.to);
	}

	private PEFNode getNode(String to) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public Object[] getElements(Object input) {
		if (pif == null || !pif.isUp()) {
			return new Object[0];
		}
		return getEdges();

	}

	private PEFEdge[] getEdges() {
		if (edges == null) {
			PrologSession s = null;
			try {
				s = pif.getSession();
				List l = s.queryAll("pef_graph_edge(From,Label,To)");
				edges = new PEFEdge[l.size()];
				int i = 0;
				for (Object object : l) {
					Map m = (Map) object;
					PEFEdge edge = new PEFEdge();
					edge.from = (String) m.get("From");
					edge.label = (String) m.get("Label");
					edge.to = (String) m.get("To");
					edges[i++] = edge;
				}
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			} finally {
				if (s != null) {
					s.dispose();
				}
			}
		}
		return edges;
	}

	@Override
	public Object getSource(Object rel) {
		PEFEdge edge = (PEFEdge) rel;
		return getNode(edge.from);
	}

	@Override
	public double getWeight(Object connection) {
		return -1;
	}

	@Override
	public void dispose() {
		if(pif!=null){
			detachFromPif();
		}
	}

	@Override
	public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
		if(oldInput==newInput){
			return;
		}
		if(pif!=null){
			detachFromPif();
		}
		reset();
		if(newInput!=null && newInput instanceof PrologInterface){
			attachToPif((PrologInterface)newInput);
		}
	}

	private void reset() {
		edges=null;
		nodes.clear();
	}
	// private Action action2;
	private final static String HOOK_ID = "PefGraphViewHook";
	private void attachToPif(PrologInterface pif) {
		this.pif=pif;
		try {
			pif.addLifeCycleHook(hook, HOOK_ID, new String[] {});
			if (pif.isUp()) {

				hook.onInit(pif,pif.getSession());

			}
			IPrologEventDispatcher d = PrologRuntimePlugin.getDefault()
					.getPrologEventDispatcher(pif);
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

		
	};

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
		}
	}

	private void removeEdge(String from, String label, String to) {

		
	}

	private void removeNode(String id) {
		// TODO Auto-generated method stub
		
	}

	private void addEdge(String from, String label, String to) {
		// TODO Auto-generated method stub
		
	}

	private void addNode(String id) {
		PEFNode node = getNode(id);
		
	}

	
	
	
}
