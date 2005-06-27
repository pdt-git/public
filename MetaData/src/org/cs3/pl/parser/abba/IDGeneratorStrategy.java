package org.cs3.pl.parser.abba;



public interface IDGeneratorStrategy {	
	public String getNodeId(String nodeType, Object key);
	public String getEdgeId(String edgeType, String edgeLabel,String fromId,String toId);
}
