package pdt.y.model;

import pdt.y.model.realizer.edges.CallEdgeRealizer;
import pdt.y.model.realizer.edges.LoadEdgeRealizer;
import pdt.y.model.realizer.groups.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.nodes.FileGroupNodeRealizer;
import pdt.y.model.realizer.nodes.PredicateNodeRealizer;
import y.base.Edge;
import y.base.EdgeMap;
import y.base.Node;
import y.layout.PortConstraint;
import y.layout.PortConstraintKeys;
import y.view.EdgeRealizer;
import y.view.Graph2D;
import y.view.NodeRealizer;
import y.view.hierarchy.DefaultHierarchyGraphFactory;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

public class GraphModel {
	private static GraphModel instance = null;
	private Graph2D graph=new Graph2D();
    
	private GraphDataHolder dataHolder=new GraphDataHolder();
	
	private HierarchyManager hierarchy = null;
	
	private NodeRealizer predicateNodeRealizer;
	private GroupNodeRealizer filegroupNodeRealizer;
	private GroupNodeRealizer moduleGroupNodeRealizer;
	
	private EdgeRealizer callEdgeRealizer;
	private EdgeRealizer loadEdgeRealizer;

		
	private GraphModel(){
		  
		initNodeRealizers();
		initEdgeRealizers();
	}
	
	
	/**
	 * Singleton Pattern
	 * This method should be used to get access to the model
	 */
	public static GraphModel getInstance() {
		if (instance == null) {
			instance = new GraphModel();
		}
		return instance;
	}

	

	private void initNodeRealizers() {
		filegroupNodeRealizer = new FileGroupNodeRealizer(this);
		moduleGroupNodeRealizer = new ModuleGroupNodeRealizer(this);
		predicateNodeRealizer = new PredicateNodeRealizer(this);    
		graph.setDefaultNodeRealizer(predicateNodeRealizer);
		//graph.setDefaultNodeRealizer(moduleGroupNodeRealizer);
	}

	private void initEdgeRealizers() {
		loadEdgeRealizer = new LoadEdgeRealizer();
		callEdgeRealizer = new CallEdgeRealizer();
		graph.setDefaultEdgeRealizer(callEdgeRealizer);
	}

	public void categorizeData() {
		categorizeNodes();		
		categorizeEdges();
	}

	private void categorizeNodes() {
		for (Node node: graph.getNodeArray()) {
			if (dataHolder.isModule(node)) {
				graph.setRealizer(node, new ModuleGroupNodeRealizer(moduleGroupNodeRealizer));
			} else if (dataHolder.isFile(node)) {
				graph.setRealizer(node, new FileGroupNodeRealizer(filegroupNodeRealizer));
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	private void categorizeEdges() {
		for (Edge edge: graph.getEdgeArray()) {
			if (dataHolder.isLoadingEdge(edge)) {
				graph.setRealizer(edge, new LoadEdgeRealizer(loadEdgeRealizer));
			} else if (dataHolder.isCallEdge(edge)) {
				CallEdgeRealizer newCallEdgeRealizer = new CallEdgeRealizer(callEdgeRealizer);
				graph.setRealizer(edge, newCallEdgeRealizer);
				newCallEdgeRealizer.adjustLineWidth();
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	
	public Graph2D getGraph() {
		return graph;
	}

	public void setGraph(Graph2D model) {
		this.graph = model;
	}
	
	public void useHierarchy(){
		if(this.hierarchy == null && this.graph !=null){
			this.hierarchy= new HierarchyManager(graph);
		}
		DefaultHierarchyGraphFactory graphFactory =(DefaultHierarchyGraphFactory)hierarchy.getGraphFactory();
		graphFactory.setDefaultGroupNodeRealizer(filegroupNodeRealizer);
		graphFactory.setProxyNodeRealizerEnabled(false);
	}
	
	public boolean isHierarchyEnabled(){
		if(hierarchy==null) 
			return false;
		
		return true;
	}
	
	public HierarchyManager getHierarchyManager(){
		return this.hierarchy;
	}
	
	
	public void clear(){
		this.graph.clear();
	}

	public String getLabelTextForNode(Node node){
		return dataHolder.getLabelTextForNode(node);
	}
	
	


	public void assignPortsToEdges() {
		EdgeMap sourceMap = graph.createEdgeMap();
		PortConstraint portConstraint = PortConstraint.create(PortConstraint.SOUTH, true);
		for (Edge edge: graph.getEdgeArray()) {
			if (dataHolder.isLoadingEdge(edge)) {
				sourceMap.set(edge, portConstraint);
			} 
		}
		graph.addDataProvider(PortConstraintKeys.SOURCE_PORT_CONSTRAINT_KEY, sourceMap);
	}


	public int getFrequency(Edge edge) {
		return dataHolder.getFrequency(edge);
	}


	public boolean isCallEdge(Edge edge) {
		return dataHolder.isCallEdge(edge);
	}


	public GraphDataHolder getDataHolder() {
		return this.dataHolder;
	}
}
