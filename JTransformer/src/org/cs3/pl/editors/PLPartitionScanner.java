package org.cs3.pl.editors;

import org.eclipse.jface.text.rules.*;

public class PLPartitionScanner extends RuleBasedPartitionScanner {
	public final static String PL_DEFAULT = "__pl_default";
	public final static String PL_COMMENT = "__pl_comment";
	public final static String PL_MULTI_COMMENT = "__pl_multi_line_comment";

	public PLPartitionScanner() {

		IToken plComment = new Token(PL_COMMENT);
		IToken plMultiComment = new Token(PL_MULTI_COMMENT);
		//IToken plPredicate = new Token(PL_DEFAULT);
		
		IPredicateRule[] rules = new IPredicateRule[2];

		rules[0] = new EndOfLineRule("%", plComment);
		rules[1] = new MultiLineRule("/*", "*/", plMultiComment);
//        rules[2] = new PredicateRule(plPredicate);

		setPredicateRules(rules);
	}
}
