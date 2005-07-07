package org.cs3.pl.parser.abba;

public interface NodeWriterStrategy {
	public final static String NODE_TYPE_PREDICATE = "predicate";
	public final static String NODE_TYPE_CLAUSE = "clause";
	public final static String NODE_TYPE_BODY_LITERAL = "literal";
	public final static String NODE_TYPE_TERM = "term";
	public static final String NODE_TYPE_CT = "ct";
	public static final String NODE_TYPE_CT_CONDITION = "ct_condition";
	public static final String NODE_TYPE_CT_ACTION = "ct_action";
	public static final String NODE_TYPE_CT_LITERAL = "ct_literal";
	
	public final static String EDGE_TYPE_CLAUSE = "clause";
	public final static String EDGE_TYPE_HEAD_LITERAL = "head_literal";
	public final static String EDGE_TYPE_BODY_LITERAL = "body_literal";
	public final static String EDGE_TYPE_ARGUMENT_TERM = "argument_term";
	public static final String EDGE_TYPE_CT_CONDITION = "ct_condition";
	public static final String EDGE_TYPE_CT_ACTION = "ct_action";
	public static final String EDGE_TYPE_CT_LITERAL = "ct_literal";
	
	
	
	public final static String PROPERTY_FILE = "file";
	public final static String PROPERTY_POSITION = "position";
	
	
	public void writeNode(String type,String nodeId,String label);
	public void writeProperty(String nodeId,String property,String[] values);
	public void writeEdge(String edgeId, String edgeType, String label, String sourceId, String targetId);
	public void writeSymTabEntry(String globalSymbol, String localId);
	public void writeRetractSymTab();
	public void writeBeginCu(String cu);
}
