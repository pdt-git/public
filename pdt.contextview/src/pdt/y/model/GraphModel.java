/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package pdt.y.model;

import pdt.y.model.realizer.edges.CallEdgeRealizer;
import pdt.y.model.realizer.edges.LoadEdgeRealizer;
import pdt.y.model.realizer.groups.FileGroupNodeRealizer;
import pdt.y.model.realizer.groups.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.nodes.PredicateNodeRealizer;
import pdt.y.model.realizer.nodes.UMLClassNodeRealizer;
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
	private Graph2D graph=new Graph2D();
    
	private GraphDataHolder dataHolder=new GraphDataHolder();
	
	private HierarchyManager hierarchy = null;
	
	private NodeRealizer predicateNodeRealizer;
	private UMLClassNodeRealizer fileNodeRealizer;
	private GroupNodeRealizer filegroupNodeRealizer;
	private GroupNodeRealizer moduleGroupNodeRealizer;
	
	private EdgeRealizer callEdgeRealizer;
	private EdgeRealizer loadEdgeRealizer;
	
	private int nodesMaxWidth;
	private int nodesMedianWidth;
	private int nodesHeight;

	public GraphModel(){
		initNodeRealizers();
		initEdgeRealizers();
	}
	
	private void initNodeRealizers() {
		filegroupNodeRealizer = new FileGroupNodeRealizer(this);
		moduleGroupNodeRealizer = new ModuleGroupNodeRealizer(this);
		predicateNodeRealizer = new PredicateNodeRealizer(this);
		fileNodeRealizer = new UMLClassNodeRealizer(this);
		setDefaultNodeRealizer(predicateNodeRealizer);
	}
	
	public void setDefaultNodeRealizer(NodeRealizer realzier) {
		graph.setDefaultNodeRealizer(realzier);
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
			} else if (dataHolder.isFileNode(node)) {
				UMLClassNodeRealizer newNodeRealizer = new UMLClassNodeRealizer(fileNodeRealizer);
				graph.setRealizer(node, newNodeRealizer);
				newNodeRealizer.initialize();
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	private void categorizeEdges() {
		for (Edge edge: graph.getEdgeArray()) {
			if (dataHolder.isLoadingEdge(edge)) {
				LoadEdgeRealizer newLoadEdgeRealizer = new LoadEdgeRealizer(loadEdgeRealizer);
				graph.setRealizer(edge, newLoadEdgeRealizer);
				
				String exports = dataHolder.getExports(edge);
				if (!exports.equals("[]")) {
					newLoadEdgeRealizer.setLabelText(exports.split(",").length + "");
				}
				
			} else if (dataHolder.isCallEdge(edge)) {
				CallEdgeRealizer newCallEdgeRealizer = new CallEdgeRealizer(callEdgeRealizer);
				graph.setRealizer(edge, newCallEdgeRealizer);
				newCallEdgeRealizer.adjustLineWidth(this);
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
	
	public int getNodesMaxWidth() {
		return nodesMaxWidth;
	}

	public void setNodesMaxWidth(int nodesMaxWidth) {
		this.nodesMaxWidth = nodesMaxWidth;
	}

	public int getNodesMedianWidth() {
		return nodesMedianWidth;
	}

	public void setNodesMedianWidth(int nodesMedianWidth) {
		this.nodesMedianWidth = nodesMedianWidth;
	}

	public int getNodesHeight() {
		return nodesHeight;
	}

	public void setNodesHeight(int nodesHeight) {
		this.nodesHeight = nodesHeight;
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


