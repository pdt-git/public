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

/*
 * Created on 30.03.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pl.parser.internal.classic;


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
