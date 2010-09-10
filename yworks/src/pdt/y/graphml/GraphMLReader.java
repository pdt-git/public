package pdt.y.graphml;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import pdt.y.model.GraphModel;
import pdt.y.model.realizer.MyShapeNodeRealizerSerializer;
import y.base.Edge;
import y.base.EdgeCursor;
import y.base.EdgeMap;
import y.base.Node;
import y.io.GraphMLIOHandler;
import y.io.graphml.KeyScope;
import y.io.graphml.KeyType;
import y.io.graphml.graph2d.Graph2DGraphMLHandler;
import y.layout.PortConstraint;
import y.layout.PortConstraintKeys;
import y.util.GraphCopier;
import y.view.Graph2D;
import y.view.Graph2DCopyFactory;


public class GraphMLReader {

	private static final Graph2D DEFAULT_EMPTY_GRAPH=new Graph2D();  

	private GraphMLIOHandler ioHandler = new GraphMLIOHandler();
	private Graph2DGraphMLHandler core = ioHandler.getGraphMLHandler();
	private GraphCopier graphCopier = new GraphCopier(new Graph2DCopyFactory());
	
	private GraphModel model = null;
	
	public GraphMLReader(){
		model = new GraphModel();
		model.useHierarchy();
				
		addInputDataAccessorsToCore();

		ioHandler.addNodeRealizerSerializer(new MyShapeNodeRealizerSerializer());
	}

	private void addInputDataAccessorsToCore() {
		core.addInputDataAcceptor("id", model.getNodeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("module", model.getModuleMap(), KeyScope.NODE,KeyType.STRING);
		core.addInputDataAcceptor("description", model.getFileNameMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("kind", model.getKindMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("functor", model.getFunctorMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("arity", model.getArityMap(), KeyScope.NODE, KeyType.INT);
	}
	
	private boolean loadFile(URL resource){
		model.clear();
		try {
			ioHandler.read(model.getGraph(), resource);
			return true;
		} catch (IOException e) {
			return false;
		}
	}
	
	
	
	public GraphModel readFile(URL resource){
		this.loadFile(resource);
		assignSouthPortToEdges();
		return this.model;
	}
	
	private void assignSouthPortToEdges() {
		Graph2D graph = model.getGraph();
		EdgeMap targetMap = graph.createEdgeMap();
		PortConstraint portConstraint = PortConstraint.create(PortConstraint.SOUTH, true);
		 for (EdgeCursor edges = graph.edges(); edges.ok(); edges.next()) {
			 Edge edge = edges.edge();
			 targetMap.set(edge, portConstraint);
		 }
		graph.addDataProvider(PortConstraintKeys.TARGET_PORT_CONSTRAINT_KEY, targetMap);
	}

	
	// For testing:
	Graph2D readFile(InputStream inputFileStream){
	    model.clear();
		try {
			ioHandler.read(model.getGraph(), inputFileStream);
		} catch (IOException e) {
			return (Graph2D) graphCopier.copy(DEFAULT_EMPTY_GRAPH);
		}
		return model.getGraph();
	}
	
	// For testing:
	String getIdForNode(Node node){
		return model.getIdForNode(node);
	}
	
	// For testing:
	String getModule(Node node){
		return model.getModule(node);
	}
	
}
