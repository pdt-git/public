package org.cs3.pl.parser.internal.term;

public class DefaultPrologTermParserVisitor implements PrologTermParserVisitor {

	public Object visit(SimpleNode node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTCompilationUnit node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTInfixTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTPrefixTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTBracesTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTListTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTParanthesisTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTCompoundTerm node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTCut node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTString node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTVariable node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTIdentifier node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTInteger node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTFloat node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTCharacters node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTPrefixOperator node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTInfixOperator node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTMember node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

}
