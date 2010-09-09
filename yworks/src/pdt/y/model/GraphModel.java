package pdt.y.model;

import pdt.y.model.realizer.CallEdgeRealizer;
import pdt.y.model.realizer.FileGroupNodeRealizer;
import pdt.y.model.realizer.LoadEdgeRealizer;
import pdt.y.model.realizer.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.MyShapeNodeRealizer;
import y.base.DataMap;
import y.base.Edge;
import y.base.Node;
import y.util.Maps;
import y.view.EdgeRealizer;
import y.view.Graph2D;
import y.view.NodeRealizer;
import y.view.hierarchy.DefaultHierarchyGraphFactory;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;

public class GraphModel {
	private Graph2D graph=new Graph2D();
    
	// Addition data:
	private DataMap nodeMap = Maps.createHashedDataMap();
	private DataMap moduleMap = Maps.createHashedDataMap();
	private DataMap fileNameMap = Maps.createHashedDataMap();
	private DataMap kindMap = Maps.createHashedDataMap();
	private DataMap functorMap = Maps.createHashedDataMap();
	private DataMap arityMap = Maps.createHashedDataMap();
	
	private HierarchyManager hierarchy = null;
	
	private NodeRealizer predicateNodeRealizer;
	private GroupNodeRealizer filegroupNodeRealizer;
	private GroupNodeRealizer moduleGroupNodeRealizer;
	
	private EdgeRealizer callEdgeRealizer;
	private EdgeRealizer loadEdgeRealizer;

	private static final String MODULE = "module";
	private static final String FILE = "file";
	private static final String PREDICATE = "predicate";
	private static final String CALL = "call";
	private static final String LOADING = "loading";
	
	public GraphModel(){
		initRealizer();
	}

	private void initRealizer() {
		initNodeRealizers();
		initEdgeRealizers();
	}

	private void initNodeRealizers() {
		filegroupNodeRealizer = new FileGroupNodeRealizer(this);
		moduleGroupNodeRealizer = new ModuleGroupNodeRealizer(this);
		predicateNodeRealizer = new MyShapeNodeRealizer(this);    
		graph.setDefaultNodeRealizer(predicateNodeRealizer);
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
			if (isModule(node)) {
				graph.setRealizer(node, new ModuleGroupNodeRealizer(moduleGroupNodeRealizer));
			} else if (isFile(node)) {
				graph.setRealizer(node, new FileGroupNodeRealizer(filegroupNodeRealizer));
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	private void categorizeEdges() {
		for (Edge edge: graph.getEdgeArray()) {
			if (isLoadingEdge(edge)) {
				graph.setRealizer(edge, new LoadEdgeRealizer(loadEdgeRealizer));
			} else if (isCallEdge(edge)) {
				graph.setRealizer(edge, new CallEdgeRealizer(callEdgeRealizer));
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	public String getIdForNode(Node node){
		return nodeMap.get(node).toString();
	}
	
	public String getModule(Node node){
		return moduleMap.get(node).toString();
	}
	
	
	
	// Getter and Setter

	public DataMap getNodeMap() {
		return nodeMap;
	}

	public DataMap getModuleMap() {
		return moduleMap;
	}

	public DataMap getFileNameMap() {
		return fileNameMap;
	}
	
	public DataMap getKindMap() {
		return kindMap;
	}

	public DataMap getFunctorMap() {
		return functorMap;
	}

	public DataMap getArityMap() {
		return arityMap;
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

	public boolean isPredicate(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(GraphModel.PREDICATE);
	}

	public boolean isModule(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(GraphModel.MODULE);
	}

	public boolean isFile(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(GraphModel.FILE);
	}
	
	public boolean isCallEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(edge).toString();
		return kind.equals(GraphModel.CALL);
	}
	
	public boolean isLoadingEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(edge).toString();
		return kind.equals(GraphModel.LOADING);
	}

	public String getLabelTextForNode(Node node) {
		String labelText;
		if (isModule(node)) {
			labelText = moduleMap.get(node).toString();
		} else if (isFile(node))  {
			labelText = fileNameMap.get(node).toString();
		} else if (isPredicate(node))  {
			labelText = functorMap.get(node) + " / " + arityMap.get(node);
		} else {
			labelText=nodeMap.get(node).toString();
		}
		return labelText;
	}
}
