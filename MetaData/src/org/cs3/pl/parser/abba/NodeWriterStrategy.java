package org.cs3.pl.parser.abba;

public interface NodeWriterStrategy {
	public final static String NODE_TYPE_PREDICATE = "predicate";
	public final static String NODE_TYPE_CLAUSE = "clause";
	public final static String NODE_TYPE_LITERAL = "literal";
	public final static String NODE_TYPE_TERM = "term";
	
	public final static String EDGE_TYPE_CLAUSE = "clause";
	public final static String EDGE_TYPE_HEAD_LITERAL = "head_literal";
	public final static String EDGE_TYPE_BODY_LITERAL = "body_literal";
	public final static String EDGE_TYPE_ARGUMENT_TERM = "argument_term";
	
	public final static String PROPERTY_FILE = "file";
	public final static String PROPERTY_POSITION = "position";
	
	public void writeNode(String type,String nodeId,String label);
	public void writeProperty(String nodeId,String property,String[] values);
	public void writeEdge(String edgeId, String edgeType, String label, String sourceId, String targetId);
}
