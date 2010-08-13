package pdt.y.graphml;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import y.base.DataAcceptor;
import y.base.DataMap;
import y.base.Graph;
import y.base.GraphCopyFactory;
import y.base.Node;
import y.io.GraphMLIOHandler;
import y.io.IOHandler;
import y.io.graphml.GraphMLHandler;
import y.io.graphml.KeyScope;
import y.io.graphml.KeyType;
import y.io.graphml.graph2d.Graph2DGraphMLHandler;
import y.io.graphml.graph2d.GroupNodeRealizerSerializer;
import y.util.GraphCopier;
import y.util.Maps;
import y.view.Graph2D;
import y.view.Graph2DCopyFactory;
import y.view.hierarchy.HierarchyManager;


public class GraphMLReader {

	private static final Graph2D DEFAULT_EMPTY_GRAPH=new Graph2D();  

	private GraphMLIOHandler ioHandler = new GraphMLIOHandler();
	private Graph2DGraphMLHandler core = ioHandler.getGraphMLHandler();
	private Graph2D graph;
	private GraphCopier graphCopier = new GraphCopier(new Graph2DCopyFactory());
	
	private DataMap dataMap = Maps.createHashedDataMap();

	private HierarchyManager hierarchy;
	
	public GraphMLReader(){
		graph = new Graph2D();
		hierarchy = new HierarchyManager(graph);
		core.addInputDataAcceptor("id", dataMap, KeyScope.NODE, KeyType.STRING);
		
		core.addNodeRealizerSerializer(new GroupNodeRealizerSerializer());
		ioHandler.addNodeRealizerSerializer(new GroupNodeRealizerSerializer());
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
	
}
