package org.cs3.pl.parser.internal.term.index;

import org.cs3.pl.parser.Index;
import org.cs3.pl.parser.internal.term.ASTCompilationUnit;
import org.cs3.pl.parser.internal.term.ASTCompoundTerm;
import org.cs3.pl.parser.internal.term.ASTIdentifier;
import org.cs3.pl.parser.internal.term.DefaultPrologTermParserVisitor;
import org.cs3.pl.parser.internal.term.SimpleNode;


/**
 * indexes all compounds/atoms.
 * @author lukas
 *
 */
public class Indexer extends DefaultPrologTermParserVisitor {

	Index index;
	String filename;
	
	public Indexer(Index index, String filename) {
		super();
		this.index = index;
		this.filename = filename;
	}

	public Object visit(ASTCompilationUnit node, Object data) {
		return traverseChildren(node.toCanonicalTerm(false,true), data);
	}

	public Object visit(ASTCompoundTerm node, Object data) {
		index.addReference(node.getFunctor(),filename);
		SimpleNode[] arguments = node.getArguments();
		for (int i = 0; i < arguments.length; i++) {
			SimpleNode arg = arguments[i];
			traverse(arg,data);
			
		}
		return data;
	}

	public Object visit(ASTIdentifier node, Object data) {
		index.addReference(node.getFunctor(),filename);
		return super.visit(node, data);
	}

}
