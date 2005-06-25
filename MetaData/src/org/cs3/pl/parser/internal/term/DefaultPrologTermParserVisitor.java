package org.cs3.pl.parser.internal.term;

public class DefaultPrologTermParserVisitor implements PrologTermParserVisitor {

	protected Object visitAllOther(SimpleNode node, Object data){
		return traverseChildren(node, data);
	}


	protected Object traverseChildren(SimpleNode node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}
	
	protected Object traverse(SimpleNode node, Object data) {
		node.jjtAccept(this, data);
		return node;
	}
	
	
	
	public Object visit(SimpleNode node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTCompilationUnit node, Object data) {			
		return visitAllOther(node,data);
	}

	public Object visit(ASTInfixTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTPrefixTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTBracesTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTListTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTParanthesisTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTCompoundTerm node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTCut node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTString node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTVariable node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTIdentifier node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTInteger node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTFloat node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTCharacters node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTPrefixOperator node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTInfixOperator node, Object data) {
		return visitAllOther(node,data);
	}

	public Object visit(ASTMember node, Object data) {
		return visitAllOther(node,data);
	}

}
