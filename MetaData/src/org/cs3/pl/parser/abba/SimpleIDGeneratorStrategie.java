package org.cs3.pl.parser.abba;

import java.util.HashMap;

public class SimpleIDGeneratorStrategie implements IDGeneratorStrategy {

	HashMap nodeIDs = new HashMap();
	
	int nextId = 0;
	public String getNodeId(String nodeType, Object key) {
		Integer id = (Integer) nodeIDs.get(key);
		if(id==null){
			id=new Integer(nextId++);
			nodeIDs.put(key,id);
		}
		return "node_id("+id.toString()+")";
	}

	public String getEdgeId(String edgeType, String edgeLabel, String fromId,
			String toId) {
	
		return "edge_id("+ (nextId++) +")";
	}

}
