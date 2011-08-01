package pdt.y.model;

import y.base.DataMap;
import y.base.Edge;
import y.base.Node;
import y.util.Maps;

public class GraphDataHolder {
	
	private static final String MODULE = "module";
	private static final String FILE = "file";
	private static final String PREDICATE = "predicate";
	private static final String CALL = "call";
	private static final String LOADING = "loading";

	
	
	// Addition data:
	private DataMap nodeMap = Maps.createHashedDataMap();
	private DataMap moduleMap = Maps.createHashedDataMap();
	private DataMap fileNameMap = Maps.createHashedDataMap();
	private DataMap kindMap = Maps.createHashedDataMap();
	private DataMap functorMap = Maps.createHashedDataMap();
	private DataMap arityMap = Maps.createHashedDataMap();
	private DataMap callFrequencyMap = Maps.createHashedDataMap();
	private DataMap dynamicMap = Maps.createHashedDataMap();
	private DataMap transparentMap = Maps.createHashedDataMap();
	private DataMap metaPredMap = Maps.createHashedDataMap();
	private DataMap multifileMap = Maps.createHashedDataMap();
	private DataMap exportedMap = Maps.createHashedDataMap();
	

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

	public DataMap getCallFrequencyMap() {
		return callFrequencyMap;
	}
	
	public DataMap getDynamicMap() {
		return dynamicMap;
	}

	public DataMap getTransparentMap() {
		return transparentMap;
	}

	public DataMap getMetaPredMap() {
		return metaPredMap;
	}

	public DataMap getMultifileMap() {
		return multifileMap;
	}

	
	public DataMap getExportedMap() {
		return exportedMap;
	}

	public boolean isPredicate(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(PREDICATE);
	}

	public boolean isModule(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(MODULE);
	}

	public boolean isFile(Node node) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(node).toString();
		return kind.equals(FILE);
	}
	
	public boolean isCallEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(edge).toString();
		return kind.equals(CALL);
	}
	
	public boolean isLoadingEdge(Edge edge) {
		DataMap kindMap = getKindMap();
		String kind = kindMap.get(edge).toString();
		return kind.equals(LOADING);
	}

	public boolean isDynamicNode(Node node) {
		Object returnNode = dynamicMap.get(node);
		if(returnNode == null) 
			return false;
		return (Boolean)returnNode;
	}
	
	public boolean isTransparentNode(Node node) {
		Object returnNode = transparentMap.get(node);
		if(returnNode == null) 
			return false;
		return (Boolean)returnNode;
	}

	public boolean isMetaPred(Node node) {
		Object returnNode = metaPredMap.get(node);
		if(returnNode == null) 
			return false;
		return (Boolean)returnNode;
	}
	
	public boolean isMultifile(Node node) {
		Object returnNode = multifileMap.get(node);
		if(returnNode == null) 
			return false;
		return (Boolean)returnNode;
	}

	public boolean isExported(Node node) {
		Object returnNode = exportedMap.get(node);
		if(returnNode == null) 
			return false;
		return (Boolean)returnNode;
	}
	
	public String getLabelTextForNode(Node node) {
		String labelText;
		if (this.isModule(node)) {
			labelText = this.getModuleName(node);
		} else if (this.isFile(node))  {
			labelText = this.getFileName(node);
		} else if (this.isPredicate(node))  {
			labelText = getPredicateText(node);
		} else {
			labelText=getNodeText(node);
		}
		return labelText;
	}

	private String getModuleName(Node node) {
		return moduleMap.get(node).toString();
	}
	
	private String getPredicateText(Node node) {
		return functorMap.get(node) + " / " + arityMap.get(node);
	}
	
	private String getFileName(Node node) {
		return fileNameMap.get(node).toString();
	}
	private String getNodeText(Node node) {
		return nodeMap.get(node).toString();
	}

	public int getFrequency(Edge edge) {
		return callFrequencyMap.getInt(edge);
	}

}
