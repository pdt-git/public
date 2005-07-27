package org.cs3.pdt.internal.editors;

import org.eclipse.jface.text.rules.EndOfLineRule;
import org.eclipse.jface.text.rules.IPredicateRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.MultiLineRule;
import org.eclipse.jface.text.rules.RuleBasedPartitionScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;

public class PLPartitionScanner extends RuleBasedPartitionScanner {
	public final static String PL_DEFAULT = "__pl_default";
	public final static String PL_COMMENT = "__pl_comment";
	public final static String PL_MULTI_COMMENT = "__pl_multi_line_comment";
	public final static String PL_SINGLE_QUOTED_STRING = "__pl__single_quoted_stringt";
	public final static String PL_DOUBLE_QUOTED_STRING = "__pl__double_quoted_stringt";

	public PLPartitionScanner() {

		IToken plComment = new Token(PL_COMMENT);
		IToken plMultiComment = new Token(PL_MULTI_COMMENT);
		IToken plSingleQuotedString = new Token(PL_SINGLE_QUOTED_STRING);
		IToken plDoubleQuotedString = new Token(PL_DOUBLE_QUOTED_STRING);
		//IToken plPredicate = new Token(PL_DEFAULT);
		
		IPredicateRule[] rules = new IPredicateRule[4];

		rules[0] = new EndOfLineRule("%", plComment);
		rules[1] = new MultiLineRule("/*", "*/", plMultiComment);
		rules[2] = new SingleLineRule("\"", "\"", plDoubleQuotedString, '\\');
		// Add a rule for single quotes
		rules[3] = new SingleLineRule("'", "'", plSingleQuotedString, '\\');
		// Add generic whitespace rule.
//        rules[2] = new PredicateRule(plPredicate);

		setPredicateRules(rules);
	}
}
