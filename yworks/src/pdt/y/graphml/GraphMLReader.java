package pdt.y.graphml;
import java.awt.Color;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import pdt.y.main.MyShapeNodeRealizer;
import pdt.y.main.MyShapeNodeRealizerSerializer;


import y.base.DataMap;
import y.base.Node;
import y.io.GraphMLIOHandler;
import y.io.graphml.KeyScope;
import y.io.graphml.KeyType;
import y.io.graphml.graph2d.Graph2DGraphMLHandler;
import y.io.graphml.graph2d.GroupNodeRealizerSerializer;
import y.util.GraphCopier;
import y.util.Maps;
import y.view.Graph2D;
import y.view.Graph2DCopyFactory;
import y.view.ShapeNodeRealizer;
import y.view.hierarchy.DefaultHierarchyGraphFactory;
import y.view.hierarchy.GroupNodeRealizer;
import y.view.hierarchy.HierarchyManager;


public class GraphMLReader {

	private static final Graph2D DEFAULT_EMPTY_GRAPH=new Graph2D();  

	private GraphMLIOHandler ioHandler = new GraphMLIOHandler();
	private Graph2DGraphMLHandler core = ioHandler.getGraphMLHandler();
	private Graph2D graph;
	private GraphCopier graphCopier = new GraphCopier(new Graph2DCopyFactory());
	
	private DataMap dataMap = Maps.createHashedDataMap();
	private DataMap moduleMap = Maps.createHashedDataMap();
	
	private HierarchyManager hierarchy;

	private ShapeNodeRealizer shapeNodeRealizer;

	private MyShapeNodeRealizer svr;
	
	public GraphMLReader(){
		graph = new Graph2D();
		shapeNodeRealizer = new ShapeNodeRealizer(ShapeNodeRealizer.ELLIPSE);
		

		GroupNodeRealizer groupNodeRealizer = new GroupNodeRealizer();
//		groupNodeRealizer.setShapeType(ShapeNodeRealizer.OCTAGON);

//		graph.setDefaultNodeRealizer(shapeNodeRealizer);

		
		  svr = new MyShapeNodeRealizer(this);
		  svr.setSize(70,70);
		  svr.setState(MyShapeNodeRealizer.FINAL_STATE);
		  svr.setFillColor(Color.ORANGE);      
		    
		graph.setDefaultNodeRealizer(svr);
		hierarchy = new HierarchyManager(graph);
		
		
//		DefaultHierarchyGraphFactory hgf =(DefaultHierarchyGraphFactory)hierarchy.getGraphFactory();
//		hgf.setDefaultGroupNodeRealizer(groupNodeRealizer);
//		hgf.setProxyNodeRealizerEnabled(false);
		
		
		core.addInputDataAcceptor("id", dataMap, KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("module", moduleMap, KeyScope.NODE,KeyType.STRING);
		
		ioHandler.addNodeRealizerSerializer(new MyShapeNodeRealizerSerializer());

	}
	
	public boolean loadFile(URL resource){
		graph.clear();
		try {
			ioHandler.read(graph, resource);
			return true;
		} catch (IOException e) {
			return false;
		}
	}
	
	
	
	public Graph2D readFile(URL resource){
		this.loadFile(resource);
		return this.graph;
	}
	
	// For testing:
	public Graph2D readFile(InputStream inputFileStream){
	    graph.clear();
		try {
			ioHandler.read(graph, inputFileStream);
		} catch (IOException e) {
			return (Graph2D) graphCopier.copy(DEFAULT_EMPTY_GRAPH);
		}
		return graph;
	}
	
	public String getIdForNode(Node node){
		return (String) dataMap.get(node);
	}
	
	public String getModule(Node node){
		return (String) moduleMap.get(node);
	}
	
}
