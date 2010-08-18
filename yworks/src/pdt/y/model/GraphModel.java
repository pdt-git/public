package pdt.y.model;

import java.awt.Color;

import pdt.y.model.realizer.MyShapeNodeRealizer;
import y.base.DataMap;
import y.base.Node;
import y.util.Maps;
import y.view.EdgeRealizer;
import y.view.Graph2D;
import y.view.NodeRealizer;
import y.view.hierarchy.HierarchyManager;

public class GraphModel {
	private Graph2D graph=new Graph2D();
    
	// Addition data:
	private DataMap dataMap = Maps.createHashedDataMap();
	private DataMap moduleMap = Maps.createHashedDataMap();
	private HierarchyManager hierarchy = null;
	
	private NodeRealizer nodeRealizer;
	private EdgeRealizer edgeRealizer;
	
	
	public GraphModel(){
		nodeRealizer = new MyShapeNodeRealizer(this);
		nodeRealizer.setSize(70,70);
		nodeRealizer.setFillColor(Color.ORANGE);      
		
		graph.setDefaultNodeRealizer(nodeRealizer);
	}
	
	
	
	
	public String getIdForNode(Node node){
		return (String) dataMap.get(node);
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

	public DataMap getDataMap() {
		return dataMap;
	}

	public void setDataMap(DataMap dataMap) {
		this.dataMap = dataMap;
	}

	public DataMap getModuleMap() {
		return moduleMap;
	}

	public void setModuleMap(DataMap moduleMap) {
		this.moduleMap = moduleMap;
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
