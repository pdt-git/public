package org.cs3.pl.parser.abba;

import java.util.Stack;

import org.cs3.pl.parser.ASTBinaryOp;
import org.cs3.pl.parser.ASTBody;
import org.cs3.pl.parser.ASTBraces;
import org.cs3.pl.parser.ASTCall;
import org.cs3.pl.parser.ASTCharAtom;
import org.cs3.pl.parser.ASTClause;
import org.cs3.pl.parser.ASTCompilationUnit;
import org.cs3.pl.parser.ASTCompound;
import org.cs3.pl.parser.ASTCut;
import org.cs3.pl.parser.ASTFloatAtom;
import org.cs3.pl.parser.ASTFunctor;
import org.cs3.pl.parser.ASTFunctorVariableModule;
import org.cs3.pl.parser.ASTHead;
import org.cs3.pl.parser.ASTIdentifier;
import org.cs3.pl.parser.ASTIntAtom;
import org.cs3.pl.parser.ASTList;
import org.cs3.pl.parser.ASTNamedCall;
import org.cs3.pl.parser.ASTParenthesis;
import org.cs3.pl.parser.ASTPredicateArgs;
import org.cs3.pl.parser.ASTPredicateSignature;
import org.cs3.pl.parser.ASTRestTokens;
import org.cs3.pl.parser.ASTSeparator;
import org.cs3.pl.parser.ASTSequence;
import org.cs3.pl.parser.ASTStringAtom;
import org.cs3.pl.parser.ASTVariable;
import org.cs3.pl.parser.PrologParserVisitor;
import org.cs3.pl.parser.SimpleNode;

public class AbbaGraphGenerator implements PrologParserVisitor {

	private NodeWriterStrategy writerStrategy;

	private IDGeneratorStrategy idStrategy;

	private Stack parentIds = new Stack();

	public AbbaGraphGenerator(NodeWriterStrategy writerStrategy,
			IDGeneratorStrategy idStrategy) {
		this.writerStrategy = writerStrategy;
		this.idStrategy = idStrategy;
	}

	public Object visit(SimpleNode node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTCompilationUnit node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTFunctor node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTFunctorVariableModule node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTVariable node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTIdentifier node, Object data) {
		String literalId = null;
		Object r=data;
		if (node.jjtGetParent() instanceof ASTBody) {
			literalId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_LITERAL,
					node);
			String parentId = (String) parentIds.peek();
			String edgeId=idStrategy.getEdgeId(NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL,null,parentId,literalId);
			writerStrategy.writeNode(NodeWriterStrategy.NODE_TYPE_LITERAL,literalId,node.getName());
			writerStrategy.writeEdge(edgeId,NodeWriterStrategy.EDGE_TYPE_BODY_LITERAL,null,parentId,literalId);
			int begin = node.getFirstToken().beginOffset;
			int end = node.getLastToken().endOffset;
			writerStrategy.writeProperty(literalId,NodeWriterStrategy.PROPERTY_POSITION,new String[]{""+begin,""+(begin-end)});
			parentIds.push(literalId);
			r = node.childrenAccept(this, data);
			parentIds.pop();
		}		
		return r;

	}

	public Object visit(ASTNamedCall node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTCall node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTPredicateSignature node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTClause node, Object data) {
		String parentId = idStrategy.getNodeId(
				NodeWriterStrategy.NODE_TYPE_PREDICATE, node.getModule() + ":"
						+ node.getName() + "/" + node.getArity());
		String clauseId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_CLAUSE,
				node);
		parentIds.push(clauseId);
		Object r = node.childrenAccept(this, data);
		parentIds.pop();
		return r;
	}

	public Object visit(ASTPredicateArgs node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTCompound node, Object data) {
		String literalId = null;
		Object r = data;
		if (node.jjtGetParent() instanceof ASTBody) {
			literalId = idStrategy.getNodeId(NodeWriterStrategy.NODE_TYPE_LITERAL,
					node);
			String parentId = (String) parentIds.peek();
			parentIds.push(literalId);
			r = node.childrenAccept(this, data);
			parentIds.pop();
		}
		
		return r;
	}

	public Object visit(ASTCut node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTSequence node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTList node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTBraces node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTParenthesis node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTRestTokens node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTIntAtom node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTFloatAtom node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTStringAtom node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTCharAtom node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTSeparator node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTBinaryOp node, Object data) {

		return node.childrenAccept(this, data);
	}

	public Object visit(ASTHead node, Object data) {
		String literalId = null;
		literalId = idStrategy
				.getNodeId(NodeWriterStrategy.NODE_TYPE_LITERAL, node);
		String parentId = (String) parentIds.peek();

		parentIds.push(literalId);
		Object r = node.childrenAccept(this, data);
		parentIds.pop();
		return r;
	}

	public Object visit(ASTBody node, Object data) {
		return node.childrenAccept(this, data);
	}

}
