/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pl.parser.abba;

import java.util.HashMap;
import java.util.Map;

import org.cs3.pl.parser.internal.term.ASTCompilationUnit;
import org.cs3.pl.parser.internal.term.ASTCompoundTerm;
import org.cs3.pl.parser.internal.term.ASTMember;
import org.cs3.pl.parser.internal.term.DefaultPrologTermParserVisitor;
import org.cs3.pl.parser.internal.term.SimpleNode;

public class AbbaGraphGenerator extends DefaultPrologTermParserVisitor {
	private NodeWriterStrategy writerStrategy;

	private IDGeneratorStrategy idStrategy;

	private Map predicates = new HashMap();

	/*
	 * a data structure describing which argument positions for a term of a
	 * given signature are potentialy evaluated (expanded) by the prolog
	 * interpreter. In adition, the struct information if such a term should be
	 * considered as a literal itself (e.g. forall/2). This is of course a very
	 * simplistic aproach. It is intended as an ad-hoc solution and should be
	 * replaced by a proper call graph analysis later.
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

	private String parentId;

	public AbbaGraphGenerator(NodeWriterStrategy writerStrategy,
			IDGeneratorStrategy idStrategy) {

		this.writerStrategy = writerStrategy;
		this.idStrategy = idStrategy;
		goalinfo.put("','/2", new GoalInfo("','/2", new int[] { 0, 1 }, false));
		goalinfo.put("';'/2", new GoalInfo("';'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'|'/2", new GoalInfo("'|'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'->'/2", new GoalInfo("'->'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'*->'/2", new GoalInfo("'*->'/2", new int[] { 0, 1 }, false));
		goalinfo.put("'\\+'/1", new GoalInfo("'\\+'/1", new int[] { 0 }, false));
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
				parentId=clauseId;
				return traverse(node.getBody().toCanonicalTerm(true, true),
						NodeWriterStrategy.NODE_TYPE_BODY_LITERAL);
			}

			String functor = node.getHeadLiteral().getFunctor();
			if ("ct/3".equals(functor)) {
				return visitCtTerm((ASTCompoundTerm) node.getHeadLiteral()
						.toCanonicalTerm(true, true), data);

			}
		}

		return data;
	}

	private Object visitCtTerm(ASTCompoundTerm ct, Object data) {
		// create a node for the ct
		ctId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CT, ct);
		SimpleNode[] args = ct.getArguments();
		String ctName = args[0].getSyntheticImage();
		writerStrategy.writeSymTabEntry(ctName, ctId);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT, ctId, ctName);
		SimpleNode original = ct.getOriginal();
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(ctId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(ctId, NodeWriterStrategy.PROPERTY_FILE,
				new String[] { "'"+cuFileName+"'" });

		// create the condition node and an edge connecting it to the ct
		SimpleNode condition = args[1];
		processCondition(ctName, condition);

		// create the action node and an edge connecting it to the ct
		SimpleNode action = args[2];
		processAction(ctName, action);

		return null;
	}

	private void processAction(String ctName, SimpleNode action) {
		SimpleNode original = action.original;
		String actionId = idStrategy.getNodeId(
				NodeWriterStrategy.NODE_TYPE_CT_ACTION, action);
		
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT_ACTION,
				actionId, ctName + "/act");
		String edgeId = idStrategy.getEdgeId(NodeWriterStrategy.EDGE_TYPE_CT_ACTION,
				null, ctId, actionId);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_CT_ACTION, null, ctId, actionId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(actionId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		
		parentId=actionId;
		traverse(action,NodeWriterStrategy.NODE_TYPE_CT_LITERAL);
	}

	private void processCondition(String ctName, SimpleNode condition) {
		SimpleNode original = condition.original;

		String conditionId = idStrategy.getNodeId(
				NodeWriterStrategy.NODE_TYPE_CT_CONDITION, condition);
		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CT_CONDITION,
				conditionId, ctName + "/cnd");
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CT_CONDITION, null, ctId,
				conditionId);
		writerStrategy.writeEdge(edgeId,
				NodeWriterStrategy.EDGE_TYPE_CT_CONDITION, null, ctId,
				conditionId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(conditionId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		
		parentId=conditionId;
		traverse(condition,NodeWriterStrategy.NODE_TYPE_CT_LITERAL);
	}

	private void processClauseHead(ASTMember node) {
		SimpleNode original = node.getOriginal();
		String functor = node.getHeadLiteral().getFunctor();
		String moduleQualifiedFunctor = node.getModuleName() + ":" + functor;

		String predicateId = getPredicateId(moduleQualifiedFunctor);

		clauseId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CLAUSE,
				node);
		String edgeId = idStrategy.getEdgeId(
				NodeWriterStrategy.EDGE_TYPE_CLAUSE, null, predicateId,
				clauseId);

		writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_CLAUSE, clauseId,
				functor);
		writerStrategy.writeEdge(edgeId, NodeWriterStrategy.EDGE_TYPE_CLAUSE,
				null, predicateId, clauseId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
		writerStrategy.writeProperty(clauseId,
				NodeWriterStrategy.PROPERTY_FILE, new String[] { "'"+cuFileName+"'" });

	}

	private String getPredicateId(String moduleQualifiedFunctor) {
		String predicateId = (String) predicates.get(moduleQualifiedFunctor);
		if (predicateId == null) {
			predicateId = idStrategy.getNodeId(
					NodeWriterStrategy.NODE_TYPE_PREDICATE,
					moduleQualifiedFunctor);
			predicates.put(moduleQualifiedFunctor, predicateId);
			writerStrategy
					.writeSymTabEntry(moduleQualifiedFunctor, predicateId);
			writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_PREDICATE,
					predicateId, moduleQualifiedFunctor);
		}
		return predicateId;
	}

	public Object visit(ASTCompoundTerm node, Object data) {

		String functor = node.getFunctor();
		GoalInfo info = (GoalInfo) goalinfo.get(functor);
		if (info == null || info.isLiteral) {
			writeLiteral(node, (String) data);
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
		writeLiteral(node, (String) data);
		return node;
	}

	private void writeLiteral(SimpleNode node, String literalType) {
		String edgeType = NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL;
		if (literalType == null) {
			literalType = NodeWriterStrategy.NODE_TYPE_BODY_LITERAL;
		}
		if (NodeWriterStrategy.NODE_TYPE_CT_LITERAL.equals(literalType)) {
			edgeType = NodeWriterStrategy.EDGE_TYPE_CT_LITERAL;
		}
		String functor = node.getFunctor();
		SimpleNode original = node.getOriginal();
		String literalId = idStrategy.getNodeId(literalType, node);
		String edgeId = idStrategy.getEdgeId(edgeType, null, parentId,
				literalId);

		writerStrategy.writeNode(literalType, literalId, functor);
		writerStrategy.writeEdge(edgeId, edgeType, null, parentId, literalId);
		int begin = original.getFirstToken().beginOffset;
		int end = original.getLastToken().endOffset;
		writerStrategy.writeProperty(literalId,
				NodeWriterStrategy.PROPERTY_POSITION, new String[] {
						"" + begin, "" + (end - begin) });
	}
}
