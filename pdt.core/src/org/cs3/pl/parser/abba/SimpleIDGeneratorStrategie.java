package org.cs3.pl.parser.abba;

import java.util.HashMap;

public class SimpleIDGeneratorStrategie implements IDGeneratorStrategy {

	HashMap nodeIDs = new HashMap();
	private static class Key{
		String type;
		Object key;
		public Key(String type,Object key) {
			this.key = key;
			this.type = type;
		}
		public boolean equals(Object obj) {
		if(obj instanceof Key){
			return type.equals(((Key)obj).type)
			&& key.equals(((Key)obj).key);
		}
		return false;
	}
	public int hashCode() {		
		return key.hashCode();
	}
	}
	int nextId = 0;
	public String getNodeId(String nodeType, Object key) {
		key = new Key(nodeType,key);
		Integer id = (Integer) nodeIDs.get(key);
		if(id==null){
			id=new Integer(nextId++);
			nodeIDs.put(key,id);
		}
		return "local_id("+id.toString()+")";
	}

	public String getEdgeId(String edgeType, String edgeLabel, String fromId,
			String toId) {
	
		return "local_id("+ (nextId++) +")";
	}

}
