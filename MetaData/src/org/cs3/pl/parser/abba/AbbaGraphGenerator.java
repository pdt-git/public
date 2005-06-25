package org.cs3.pl.parser.abba;

import java.util.HashMap;
import java.util.Map;
import java.util.Stack;

import org.cs3.pl.parser.internal.term.ASTCompilationUnit;
import org.cs3.pl.parser.internal.term.ASTCompoundTerm;
import org.cs3.pl.parser.internal.term.ASTMember;
import org.cs3.pl.parser.internal.term.DefaultPrologTermParserVisitor;
import org.cs3.pl.parser.internal.term.SimpleNode;

import sun.security.action.GetBooleanAction;

public class AbbaGraphGenerator extends DefaultPrologTermParserVisitor {
	private NodeWriterStrategy writerStrategy;

	private IDGeneratorStrategy idStrategy;

	private Stack parentIds = new Stack();

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
		return traverseChildren(node.toCanonicalTerm(true, true), data);
	}

	public Object visit(ASTMember node, Object data) {
		SimpleNode original = node.getOriginal();
		if (!node.isDirective()) {

			String functor = node.getHeadLiteral().getFunctor();
			String parentId = idStrategy.getNodeId(
					NodeWriterStrategy.NODE_TYPE_PREDICATE, node
							.getModuleName()
							+ ":" + functor);
			clauseId = idStrategy.getNodeId(
					NodeWriterStrategy.NODE_TYPE_CLAUSE, node);
			String edgeId = idStrategy.getEdgeId(
					NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, parentId,
					clauseId);

			writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CLAUSE,
					clauseId, functor);
			writerStrategy.writeEdge(edgeId,
					NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, parentId,
					clauseId);
			int begin = original.getFirstToken().beginOffset;
			int end = original.getLastToken().endOffset;
			writerStrategy.writeProperty(clauseId,
					NodeWriterStrategy.PROPERTY_POSITION, new String[] {
							"" + begin, "" + (begin - end) });
			writerStrategy.writeProperty(clauseId,
					NodeWriterStrategy.PROPERTY_FILE,
					new String[] { cuFileName });

			if (!node.isFact()) {
				return traverse(node.getBody(), data);
			}
		}

		return data;
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
						"" + begin, "" + (begin - end) });
	}
}
