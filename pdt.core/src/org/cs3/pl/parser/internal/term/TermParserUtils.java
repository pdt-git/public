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

package org.cs3.pl.parser.internal.term;

import java.io.StringReader;
import java.util.Vector;

import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.parser.Problem;

public class TermParserUtils {
	static Problem createProblem(SimpleNode node, String message, int severity) {
		Problem p = new Problem();
		Token first = node.getFirstToken();
		Token last = node.getLastToken();
		p.beginOffset = first.beginOffset;
		p.endOffset = last.endOffset;
		p.firstColumn = first.beginColumn;
		p.firstRow = first.beginLine;
		p.lastColumn = last.endColumn;
		p.lastRow = last.endLine;
		p.message = message;
		p.severity = severity;
		return p;
	}

	static SourceLocation createSourceLocation(SimpleNode node,
			ASTCompilationUnit root) {
		SourceLocation r = new SourceLocation(root.getFilename(), true, false);
		r.offset = node.getBeginToken().beginOffset;
		r.endOffset = node.getLastToken().endOffset;
		return r;
	}

	public static boolean isExportDeclaration(SimpleNode n){
		return isPredicateSignature(n);
	}

	public static boolean isPredicateSignature(SimpleNode n) {
		ASTCompoundTerm t = ASTCompoundTerm.cast(n);
		if(t==null){			
			n=n.toCanonicalTerm(false,true);
		}
		t = ASTCompoundTerm.cast(n);
		if(t==null){
			return false;
		}
		if(!"/".equals(t.getLabel())){
			return false;
		}
		if(t.getArity()!=2){
			return false;
		}
		SimpleNode[] arguments = t.getArguments();
		if(!(arguments[1] instanceof ASTInteger)){
			return false;
		}
		if(!(arguments[0] instanceof Atomic)){
			return false;
		}
		return true;
	}
	
	public static boolean isModuleQualifiedPredicateSignature(SimpleNode n){
		ASTCompoundTerm t = ASTCompoundTerm.cast(n);
		if(t==null){			
			n=n.toCanonicalTerm(false,true);
		}
		t = ASTCompoundTerm.cast(n);
		if(t==null){
			return false;
		}
		if(!":".equals(t.getLabel())){
			return false;
		}
		if(t.getArity()!=2){
			return false;
		}
		SimpleNode[] arguments = t.getArguments();
		if(!(arguments[0] instanceof Atomic)){
			return false;
		}
		if(!isPredicateSignature(arguments[1])){
			return false;
		}
		return true;
	}

	public static boolean shouldBeQuoted(String atomValue) {
		//TODO: when exactly?
		if(true){
			return true;
		}
		OPS ops = new OPS();
		if(ops.isInfixOp(atomValue)||ops.isPrefixOp(atomValue)){
			return true;
		}
		if(atomValue.indexOf(",")>=0){
			return true;
		}
		if(atomValue.indexOf("$")>=0){
			return true;
		}
		if(atomValue.indexOf(".")>=0){
			return true;
		}
		if(atomValue.indexOf("\"")>=0){
			return true;
		}
		if(atomValue.indexOf("\'")>=0){
			return true;
		}
		Token[] tokens;
		try{			
			tokens = tokenize(atomValue);
		}catch(Throwable e){
			return true;
		}
		if(tokens.length!=1){
			return true;
		}
		if(tokens[0].kind==PrologTermParser.CUT){
			return false;
		}
		if(tokens[0].kind==PrologTermParser.OPERATOR){
			return false;
		}
		if(tokens[0].kind==PrologTermParser.IDENTIFIER){
			return false;
		}
		return true;
	}

	public static Token[] tokenize(String input) {
		Vector v = new Vector();
		PrologTermParserTokenManager tm = new PrologTermParserTokenManager(
				new SimpleCharStream(new StringReader(input)));
		for(Token t= tm.getNextToken();t.kind!=PrologTermParser.EOF;t=tm.getNextToken()){
			v.add(t);
		}
		return (Token[]) v.toArray(new Token[v.size()]);
	}

	public static void main(String[] args) {
		System.out.println(shouldBeQuoted("Assumption failed: ~p"));
		System.exit(0);
	}

	public static boolean isSignature(SimpleNode n) {
		return isPredicateSignature(n)||isModuleQualifiedPredicateSignature(n);
	}
}
