package pdt.y.graphml;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;

import pdt.y.model.GraphDataHolder;
import pdt.y.model.GraphModel;
import pdt.y.model.realizer.nodes.PredicateNodeRealizerSerializer;
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
		GraphDataHolder dataHolder = model.getDataHolder();
		core.addInputDataAcceptor("id", dataHolder.getNodeMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("module", dataHolder.getModuleMap(), KeyScope.NODE,KeyType.STRING);
		core.addInputDataAcceptor("description", dataHolder.getFileNameMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("kind", dataHolder.getKindMap(), KeyScope.ALL, KeyType.STRING);
		core.addInputDataAcceptor("functor", dataHolder.getFunctorMap(), KeyScope.NODE, KeyType.STRING);
		core.addInputDataAcceptor("arity", dataHolder.getArityMap(), KeyScope.NODE, KeyType.INT);
		core.addInputDataAcceptor("frequency", dataHolder.getCallFrequencyMap(), KeyScope.EDGE, KeyType.INT);
		core.addInputDataAcceptor("isDynamic", dataHolder.getDynamicMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isTransparent", dataHolder.getTransparentMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isMultifile", dataHolder.getMultifileMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isMetaPredicate", dataHolder.getMetaPredMap(), KeyScope.NODE, KeyType.BOOLEAN);
		core.addInputDataAcceptor("isExported", dataHolder.getExportedMap(), KeyScope.NODE, KeyType.BOOLEAN);
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
	

	
}
