package org.cs3.pl.parser.abba.tests;

import java.io.InputStream;
import java.util.Iterator;
import java.util.List;

import junit.framework.TestCase;

import org.cs3.pl.parser.ASTClause;
import org.cs3.pl.parser.ASTCompilationUnit;
import org.cs3.pl.parser.PrologParser;
import org.cs3.pl.parser.SimpleNode;
import org.cs3.pl.parser.abba.AbbaGraphGenerator;

public class CollectorTest extends TestCase {
	public void testCollectLiterals() throws Exception {
		InputStream stream = CollectorTest.class.getResourceAsStream("collector_test.pl");
		PrologParser parser = new PrologParser(stream);
		parser.CompilationUnit();
		ASTCompilationUnit root = parser.getASTRoot();
//		AbbaGraphGenerator visitor = new AbbaGraphGenerator();
//		root.childrenAccept(visitor, null);
//		List l = visitor.abbaLiterals;
//		for (Iterator iter = l.iterator(); iter.hasNext();) {
//			SimpleNode ident = (SimpleNode) iter.next();
//			System.out.println(ident.getImage());
//		}
//		l=visitor.abbaClauses;
//		for (Iterator iter = l.iterator(); iter.hasNext();) {
//			SimpleNode clause = (SimpleNode) iter.next();
//			System.out.println(clause.getImage());
//		}
		
		
	}
}
