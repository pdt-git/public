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

import java.io.File;
import java.util.Arrays;

import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;

import pdt.y.model.realizer.edges.CallEdgeRealizer;
import pdt.y.model.realizer.edges.LoadEdgeRealizer;
import pdt.y.model.realizer.groups.FileGroupNodeRealizer;
import pdt.y.model.realizer.groups.ModuleGroupNodeRealizer;
import pdt.y.model.realizer.nodes.NodeRealizerBase;
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
	private NodeRealizerBase fileNodeRealizer;
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
		graph.setDefaultNodeRealizer(predicateNodeRealizer);
	}

	private void initEdgeRealizers() {
		loadEdgeRealizer = new LoadEdgeRealizer(this);
		callEdgeRealizer = new CallEdgeRealizer();
		graph.setDefaultEdgeRealizer(callEdgeRealizer);
	}

	private void analyzeGraph() {
		if (graph.getNodeArray().length == 0)
			return;
		
		NodeRealizerBase realizer = (NodeRealizerBase)graph.getDefaultNodeRealizer();
		int i = 0;
		int[] lengths = new int[graph.getNodeArray().length];
		
		for (Node node: graph.getNodeArray()) {
			String text = getLabelTextForNode(node);
			
			int v = realizer.calcLabelSize(text).getWidth() + 14;
			lengths[i++] = v;
		}
		
		Arrays.sort(lengths);
		
		int maxWidth = lengths[i - 1];
		int medianWidth = lengths[i / 2];
		setNodesMaxWidth(maxWidth);
		setNodesMedianWidth(medianWidth);
	}

	public void categorizeData() {
		analyzeGraph();
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
				PredicateNodeRealizer newNodeRealizer = new PredicateNodeRealizer(predicateNodeRealizer);
				graph.setRealizer(node, newNodeRealizer);
				newNodeRealizer.fitContent();
			}
		}
	}

	private void categorizeEdges() {
		for (Edge edge: graph.getEdgeArray()) {
			if (dataHolder.isLoadingEdge(edge)) {
				LoadEdgeRealizer newLoadEdgeRealizer = new LoadEdgeRealizer(loadEdgeRealizer);
				graph.setRealizer(edge, newLoadEdgeRealizer);
				
				String edgeLabel = dataHolder.getEdgeLabel(edge);
				if (edgeLabel != null && !edgeLabel.isEmpty()) {
					newLoadEdgeRealizer.setLabelText(edgeLabel);
				}
				
			} else if (dataHolder.isCallEdge(edge)) {
				CallEdgeRealizer newCallEdgeRealizer = new CallEdgeRealizer(callEdgeRealizer);
				graph.setRealizer(edge, newCallEdgeRealizer);
				newCallEdgeRealizer.adjustLineWidth(this);
				
				String label = calculateLabel(edge);
				
				newCallEdgeRealizer.setLabelText(label);
			} else {
				// no realizer to set because it is already bound to default realizer
			}
		}
	}

	private String calculateLabel(Edge edge) {
		try {
			String filename = dataHolder.getFileName(edge);
			String offset = dataHolder.getOffset(edge);
			IDocument doc = UIUtils.getDocument(new File(filename));
			
			String[] parts = offset.split("-");
			int start = Integer.parseInt(parts[0]);
			int end = Integer.parseInt(parts[1]);
			
			int physicalstart = UIUtils.logicalToPhysicalOffset(doc, start);
			int physicalend = UIUtils.logicalToPhysicalOffset(doc, end);
			
			String literal = doc.get(physicalstart, physicalend-physicalstart);
			String parentLiteral = getParentLiteral(doc, physicalstart, physicalend, literal);
			return literal.compareTo(parentLiteral) == 0 ? "" : parentLiteral;
			
		} catch (Exception ex) {
			ex.printStackTrace();
		}
		return "";
	}

	private String getParentLiteral(IDocument doc, int physicalstart, int physicalend, String literal) throws BadLocationException {
		String res = literal;
		
		// right side
		char c;
		int inside_parenthesis = 0;
		boolean inside_atom = false;
		int rightoffset = physicalend;
		right_side: while (true)
		{
			c = doc.getChar(rightoffset);
			boolean ignore = inside_parenthesis > 0 || inside_atom;
			
			switch (c)
			{
			case '.':
				if (!ignore) return literal;
				break;
				
			case ')':
				if (!inside_atom) {
					if (inside_parenthesis > 0) inside_parenthesis--;
					else {
						res = res + ")";
						break right_side;
					}
				}
				break;
				
			case '(':
				if (!inside_atom) inside_parenthesis++;
				break;
				
			case '\'':
				inside_atom = !inside_atom;
				break;
				
			case ',':
				if (!ignore) res = res + ", ... ";
				break;
			}
			rightoffset++;
		}
		
		// left side
		inside_parenthesis = 0;
		inside_atom = false;
		int leftoffset = physicalstart - 1;
		left_side: while (true)
		{
			c = doc.getChar(leftoffset);
			boolean ignore = inside_parenthesis > 0 || inside_atom;
			
			switch (c)
			{
			case ':':
				if (!ignore && doc.getChar(leftoffset + 1) == '-') return literal;
				break;
				
			case '(':
				if (!inside_atom) {
					if (inside_parenthesis > 0) inside_parenthesis--;
					else {
						String name = "";
						char ci;
						while (Character.isLetterOrDigit(ci = doc.getChar(--leftoffset)) || ci == '_') name = ci + name;
						res = name + "(" + res;
						break left_side;
					}
				}
				break;
				
			case ')':
				if (!inside_atom) inside_parenthesis++;
				break;
				
			case '\'':
				inside_atom = !inside_atom;
				break;
				
			case ',':
				if (!ignore) res = " ... ," + res;
				break;
			}
			leftoffset--;
		}
		return getParentLiteral(doc, leftoffset, rightoffset, res);
	}

	public Graph2D getGraph() {
		return graph;
	}

	public void setGraph(Graph2D graph) {
		this.graph = graph;
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


