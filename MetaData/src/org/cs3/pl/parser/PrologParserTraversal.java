/*
 * Created on 30.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser;


/**
 * @author windeln
 *
 * checks if
 * 
 * all variables in post action are always bound by the precondition 
 */
public class PrologParserTraversal implements PrologParserVisitor {

	public Object visit(SimpleNode node, Object data) {
		throw new RuntimeException("node type unknown: " + node.getClass());
	}

	public Object visit(ASTCompilationUnit node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTFunctor node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTVariable node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}
	public Object visit(ASTNamedCall node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTCall node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTPredicateSignature node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTClause node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTPredicateHead node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTDividedAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTSequence node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTList node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTBinaryOp node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTParenthesis node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTCut, java.lang.Object)
	 */
	public Object visit(ASTCut node, Object data) {
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTCompound, java.lang.Object)
	 */
	public Object visit(ASTCompound node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTIntAtom, java.lang.Object)
	 */
	public Object visit(ASTIntAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTFloatAtom, java.lang.Object)
	 */
	public Object visit(ASTFloatAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTStringAtom, java.lang.Object)
	 */
	public Object visit(ASTStringAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTFunctorVariableModule, java.lang.Object)
	 */
	public Object visit(ASTFunctorVariableModule node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTRestTokens, java.lang.Object)
	 */
	public Object visit(ASTRestTokens node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTIdentifier, java.lang.Object)
	 */
	public Object visit(ASTIdentifier node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTCharAtom, java.lang.Object)
	 */
	public Object visit(ASTCharAtom node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTSeparator, java.lang.Object)
	 */
	public Object visit(ASTSeparator node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTInitialization, java.lang.Object)
	 */
	public Object visit(ASTInitialization node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTBraces, java.lang.Object)
	 */
	public Object visit(ASTBraces node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTPredicateArgs, java.lang.Object)
	 */
	public Object visit(ASTPredicateArgs node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	/* (non-Javadoc)
	 * @see org.cs3.pl.parser.PrologParserVisitor#visit(org.cs3.pl.parser.ASTBody, java.lang.Object)
	 */
	public Object visit(ASTBody node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}

	public Object visit(ASTHead node, Object data) {
		node.childrenAccept(this, data);
		return node;
	}
}
