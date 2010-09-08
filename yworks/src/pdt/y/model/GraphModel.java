package pdt.y.model;

import java.awt.Color;

import pdt.y.model.realizer.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.MyGroupNodeRealizer;
import pdt.y.model.realizer.MyShapeNodeRealizer;
import y.base.DataMap;
import y.base.Node;
import y.util.Maps;
import y.view.Arrow;
import y.view.EdgeRealizer;
import y.view.GenericEdgeRealizer;
import y.view.Graph2D;
import y.view.LineType;
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
	private HierarchyManager hierarchy = null;
	
	private NodeRealizer nodeRealizer;
	private EdgeRealizer edgeRealizer;

	private GroupNodeRealizer filegroupNodeRealizer;
	private GroupNodeRealizer moduleGroupNodeRealizer;
	
	public GraphModel(){
		initGroupNodeRealizer();
		initModuleGroupNodeRealizer();
		initNodeRealizer();
		initEdgeNodeRealizer();
	}
	
	private void initGroupNodeRealizer() {
		filegroupNodeRealizer = new MyGroupNodeRealizer(this);
	}
	
	private void initModuleGroupNodeRealizer() {
		moduleGroupNodeRealizer = new ModuleGroupNodeRealizer(this);
	}
	
	
	private void initNodeRealizer() {
		nodeRealizer = new MyShapeNodeRealizer(this);    
		graph.setDefaultNodeRealizer(nodeRealizer);
	}

	private void initEdgeNodeRealizer() {
		edgeRealizer = new GenericEdgeRealizer();
		edgeRealizer.setTargetArrow(Arrow.DELTA);
		edgeRealizer.setLineColor(Color.BLUE);
		byte myStyle = LineType.LINE_3.getLineStyle();
		LineType myLineType = LineType.getLineType(2,myStyle);
		edgeRealizer.setLineType(myLineType);
		graph.setDefaultEdgeRealizer(edgeRealizer);
	}

	public String getIdForNode(Node node){
		return (String) nodeMap.get(node);
	}
	
	public String getModule(Node node){
		return (String) moduleMap.get(node);
	}
	
	
	
	// Getter and Setter
	
	public NodeRealizer getNodeRealizer() {
		return nodeRealizer;
	}

	public void setNodeRealizer(NodeRealizer nodeRealizer) {
		this.nodeRealizer = nodeRealizer;
		this.graph.setDefaultNodeRealizer(nodeRealizer);
	}

	public EdgeRealizer getEdgeRealizer() {
		return edgeRealizer;
	}

	public void setEdgeRealizer(EdgeRealizer edgeRealizer) {
		this.edgeRealizer = edgeRealizer;
		this.graph.setDefaultEdgeRealizer(edgeRealizer);
	}

	public DataMap getNodeMap() {
		return nodeMap;
	}

	public void setNodeMap(DataMap dataMap) {
		this.nodeMap = dataMap;
	}

	public DataMap getModuleMap() {
		return moduleMap;
	}

	public void setModuleMap(DataMap moduleMap) {
		this.moduleMap = moduleMap;
	}

	public DataMap getFileNameMap() {
		return fileNameMap;
	}

	public void setFileNameMap(DataMap fileNameMap) {
		this.fileNameMap = fileNameMap;
	}

	public DataMap getKindMap() {
		return kindMap;
	}

	public void setKindMap(DataMap kindMap) {
		this.kindMap = kindMap;
	}

	public Graph2D getGraph() {
		return graph;
	}

	public void setGraph(Graph2D model) {
		this.graph = model;
	}
	
	
	
	public GroupNodeRealizer getFilegroupNodeRealizer() {
		return filegroupNodeRealizer;
	}

	public GroupNodeRealizer getModuleGroupNodeRealizer() {
		return moduleGroupNodeRealizer;
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
}
