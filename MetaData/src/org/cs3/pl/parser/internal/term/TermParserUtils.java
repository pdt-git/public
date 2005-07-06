package org.cs3.pl.parser.internal.term;

import java.io.StringReader;
import java.util.Vector;

import org.cs3.pl.metadata.PrologElementData;
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
