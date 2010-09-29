package pdt.y.graphml;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import pdt.y.model.GraphModel;
import pdt.y.model.realizer.PredicateNodeRealizerSerializer;
import y.base.Node;
import y.io.GraphMLIOHandler;
import y.io.graphml.KeyScope;
import y.io.graphml.KeyType;
import y.io.graphml.graph2d.Graph2DGraphMLHandler;
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
		model = GraphModel.getInstance();
		model.useHierarchy();
				
		addInputDataAccessorsToCore();

		ioHandler.addNodeRealizerSerializer(new PredicateNodeRealizerSerializer());
	}

	private void addInputDataAccessorsToCore() {
		core.addInputDataAcceptor("id", model.getNodeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("module", model.getModuleMap(), KeyScope.NODE,KeyType.STRING);
		core.addInputDataAcceptor("description", model.getFileNameMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("kind", model.getKindMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("functor", model.getFunctorMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("arity", model.getArityMap(), KeyScope.NODE, KeyType.INT);
		core.addInputDataAcceptor("frequency", model.getCallFrequencyMap(), KeyScope.EDGE, KeyType.INT);
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
		//model.assignSouthPortToEdges();
		return this.model;
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
