package org.cs3.pl.editors;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.cs3.pl.Debug;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;

public class PLScanner extends RuleBasedScanner {
	//public static final String[] plKeywords = {"assert", "retract", "print","write"};
	public static String[] plKeywords = null;
	//{"assert", "retract", "print","write","functor",
		//	"atom_concat","explain","retractall","nth_clause","atom_to_term","atom_to_term",
			//"number", "bagof","findall"};
    private String[] plDynamicPredicates = null;

	private void initDynamicPredicates() {
	    if (plDynamicPredicates == null){
	        IPrologClient client;
            try {
                client = PrologManager.getInstance().getHiddenClient();
                Hashtable solution = client.query("predicate_property(P,dynamic),functor(P,Name,_)");
                List keywords = new ArrayList();
                while(solution != null) {
                    keywords.add(solution.get("Name"));
                    solution = client.next();
                }
                plDynamicPredicates = (String[])keywords.toArray(new String[0]);
            } catch (IOException e) {
                plDynamicPredicates = new String[0];
                Debug.report(e);
            }
	    }
	}

	
	
	private void initKeywords() {
	    if (plKeywords == null){
	        IPrologClient client;
            try {
                client = PrologManager.getInstance().getHiddenClient();
                Hashtable solution = client.query("predicate_property(P,built_in),functor(P,Name,_)");
                List keywords = new ArrayList();
                while(solution != null) {
                    keywords.add(solution.get("Name"));
                    solution = client.next();
                }
                plKeywords = (String[])keywords.toArray(new String[0]);
            } catch (IOException e) {
                plKeywords = new String[0];
                Debug.report(e);
            }
	    }
	}
	 
	public PLScanner(ColorManager manager) {
	    initKeywords();
	    initDynamicPredicates();
		IToken procInstr =
			new Token(
				new TextAttribute(
					manager.getColor(IPLColorConstants.PL_VAR), null, 1));
		IToken keywordToken =
			new Token(
					new TextAttribute(
							manager.getColor(IPLColorConstants.PL_KEYWORD), null, 1));
		
		IToken dynamicToken =
			new Token(
					new TextAttribute(
							manager.getColor(IPLColorConstants.PL_DYNAMIC), null, 1));
		IToken wordToken =
			new Token(
					new TextAttribute(
							manager.getColor(IPLColorConstants.PL_KEYWORD), null, 0));
		
		IToken string =
			new Token(
					new TextAttribute(manager.getColor(IPLColorConstants.STRING)));
		
		IRule[] rules = new IRule[5];
		//Add rule for processing instructions

		WordRule wordRule = new WordRule(new WordDetector(), wordToken);
		WordRule dynamicRule = new WordRule(new WordDetector(), dynamicToken);
		
		for (int i = 0; i < plKeywords.length; i++)
			wordRule.addWord(plKeywords[i], keywordToken);
		for (int i = 0; i < plDynamicPredicates.length; i++)
			wordRule.addWord(plDynamicPredicates[i], dynamicToken);
		
		rules[0] = new VarRule(procInstr);
		// Add rule for double quotes
		rules[1] = new SingleLineRule("\"", "\"", string, '\\');
		// Add a rule for single quotes
		rules[2] = new SingleLineRule("'", "'", string, '\\');
		// Add generic whitespace rule.
		rules[3] = new WhitespaceRule(new PLWhitespaceDetector());
		rules[4] = wordRule; 
		//rules[5] = dynamicRule; 
		

		setRules(rules);
	}
}
