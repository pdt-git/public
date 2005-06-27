package org.cs3.pl.parser.abba;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import org.cs3.pl.parser.internal.term.ASTCompilationUnit;
import org.cs3.pl.parser.internal.term.ASTCompoundTerm;
import org.cs3.pl.parser.internal.term.ASTMember;
import org.cs3.pl.parser.internal.term.DefaultPrologTermParserVisitor;
import org.cs3.pl.parser.internal.term.SimpleNode;

public class AbbaGraphGenerator extends DefaultPrologTermParserVisitor {
	private NodeWriterStrategy writerStrategy;

	private IDGeneratorStrategy idStrategy;

	private Map predicates=new HashMap();
	
	

	/*
	 * a data structure describing which argument positions for a term of a
	 * given signature are potentialy evaluated (expanded) by the prolog
	 * interpreter. In adition, the struct information if such a term should be
	 * considered as a literal itself (e.g. forall/2).
	 * 
	 * This is of course a very simplistic aproach. It is intended as an ad-hoc
	 * solution and should be replaced by a proper call graph analysis later.
	 */
	private static class GoalInfo {
		final String functor;

		final int[] activePositions;

		final boolean isLiteral;

		public GoalInfo(String functor, int[] positions, boolean literal) {
			super();
			activePositions = positions;
			this.functor = functor;
			isLiteral = literal;
		}

	}

	private String cuModule;

	private String cuFileName;

	private Map goalinfo = new HashMap();

	private String clauseId;

	private String ctId;

	public AbbaGraphGenerator(NodeWriterStrategy writerStrategy,
			IDGeneratorStrategy idStrategy) {

		this.writerStrategy = writerStrategy;
		this.idStrategy = idStrategy;
		goalinfo.put("','/2", new GoalInfo("','/2", new int[] { 0, 1 }, false));
		goalinfo.put(";/2", new GoalInfo(";/2", new int[] { 0, 1 }, false));
		goalinfo.put("|/2", new GoalInfo("|/2", new int[] { 0, 1 }, false));
		goalinfo.put("->/2", new GoalInfo("->/2", new int[] { 0, 1 }, false));
		goalinfo.put("*->/2", new GoalInfo("*->/2", new int[] { 0, 1 }, false));
		goalinfo.put("\\+/1", new GoalInfo("\\+/1", new int[] { 0 }, false));
		goalinfo.put("call/1", new GoalInfo("call/1", new int[] { 0 }, false));
		goalinfo.put("not/1", new GoalInfo("not/1", new int[] { 0 }, false));
		goalinfo.put("ignore/1", new GoalInfo("ignore/1", new int[] { 0 },
				false));
		goalinfo.put("call_with_depth_limit/3", new GoalInfo(
				"call_with_depth_limit/3", new int[] { 0 }, true));
		goalinfo.put("call_cleanup/3", new GoalInfo("call_cleanup/3",
				new int[] { 0, 2 }, true));
		goalinfo.put("call_cleanup/2", new GoalInfo("call_cleanup/2",
				new int[] { 0, 1 }, true));
		goalinfo.put("catch/3", new GoalInfo("catch/3", new int[] { 0, 2 },
				true));
		goalinfo.put("block/3", new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("findall/3",
				new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("setof/3", new GoalInfo("block/3", new int[] { 1 }, true));
		goalinfo.put("bagof/3", new GoalInfo("bagof/3", new int[] { 1 }, true));
		goalinfo.put("forall/2", new GoalInfo("forall/2", new int[] { 0, 1 },
				true));
	}

	public Object visit(ASTCompilationUnit node, Object data) {
		this.cuModule = node.getModuleName();
		this.cuFileName = node.getFilename();
		return traverseChildren(node, data);
	}

	public Object visit(ASTMember node, Object data) {
		
		if (!node.isDirective()) {

			 processClauseHead(node);

			if (!node.isFact()) {
				return traverse(node.getBody().toCanonicalTerm(true,true), data);
			}

			String functor = node.getHeadLiteral().getFunctor();
			if ("ct/3".equals(functor)){
				return visitCtTerm((ASTCompoundTerm) node.getHeadLiteral(),data);
				
			}
		}

		return data;
	}

	private Object visitCtTerm(ASTCompoundTerm ct, Object data) {
		ctId=idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CT,ct);
		SimpleNode[] args = ct.getArguments();
		String ctName=args[0].getLabel();
		writerStrategy.writeSymTabEntry(ctName,ctId);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT,ctId,ctName);
		
		String conditionId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CT_CONDITION,args[1]);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT_CONDITION,conditionId,ctName+"/cnd");
		String edgeId=idStrategy.getEdgeId(NodeWriterStrategy.EDGE_TYPE_CT_CONDITION,null,ctId,conditionId);
		writerStrategy.writeEdge(edgeId,NodeWriterStrategy.EDGE_TYPE_CT_CONDITION,null,ctId,conditionId);
		String actionId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CT_ACTION,args[2]);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT_ACTION,actionId,ctName+"/act");
		edgeId=idStrategy.getEdgeId(NodeWriterStrategy.EDGE_TYPE_CT_ACTION,null,ctId,actionId);
		writerStrategy.writeEdge(edgeId,NodeWriterStrategy.EDGE_TYPE_CT_ACTION,null,ctId,actionId);
		
		return null;
	}

	private void processClauseHead(ASTMember node) {
		SimpleNode original = node.getOriginal();	
		String functor = node.getHeadLiteral().getFunctor();
		String moduleQualifiedFunctor = node
		.getModuleName()
		+ ":" + functor;

		String predicateId = getPredicateId(moduleQualifiedFunctor);				
		
		clauseId = idStrategy.getNodeId(
				NodeWriterStrategy.NODE_TYPE_CLAUSE, node);
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, predicateId,
				clauseId);

		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CLAUSE,
				clauseId, functor);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, predicateId,
				clauseId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end-begin) });
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_FILE,
				new String[] { cuFileName });
		
	}

	private String getPredicateId(String moduleQualifiedFunctor) {
		String predicateId = (String) predicates.get(moduleQualifiedFunctor); 
			if(predicateId==null){
				predicateId=idStrategy.getNodeId(
						NodeWriterStrategy.NODE_TYPE_PREDICATE, moduleQualifiedFunctor);
				predicates.put(moduleQualifiedFunctor,predicateId);
				writerStrategy.writeSymTabEntry(moduleQualifiedFunctor,predicateId);
				writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_PREDICATE,predicateId,moduleQualifiedFunctor);
			}
		return predicateId;
	}
	
	

	public Object visit(ASTCompoundTerm node, Object data) {

		String functor = node.getFunctor();
		GoalInfo info = (GoalInfo) goalinfo.get(functor);
		if (info == null || info.isLiteral) {
			writeLiteral(node);
		}
		if (info != null) {
			for (int i = 0; i < info.activePositions.length; i++) {
				int pos = info.activePositions[i];
				traverse(node.getArguments()[pos], data);
			}
		}
		return node;
	}

	protected Object visitAllOther(SimpleNode node, Object data) {
		writeLiteral(node);
		return node;
	}
	
	private void writeLiteral(SimpleNode node) {
		String functor = node.getFunctor();
		SimpleNode original = node.getOriginal();
		String literalId = idStrategy.getNodeId(
				NodeWriterStrategy.NODE_TYPE_LITERAL, node);
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL, null, clauseId,
				literalId);

		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_LITERAL,
				literalId, functor);
		writerStrategy.writeEdge(edgeId, NodeWriterStrategy.EDGE_TYPE_CLAUSE,
				null, clauseId, literalId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end-begin) });
	}
}
