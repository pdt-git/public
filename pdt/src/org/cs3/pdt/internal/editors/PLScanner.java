package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.ui.IFileEditorInput;

public class PLScanner extends RuleBasedScanner {
	// public static final String[] plKeywords = {"assert", "retract",
	// "print","write"};
	public static String[] plKeywords = null;

	// {"assert", "retract", "print","write","functor",
	// "atom_concat","explain","retractall","nth_clause","atom_to_term","atom_to_term",
	// "number", "bagof","findall"};
	private String[] plDynamicPredicates = null;

	private void initDynamicPredicates(IPrologProject plProject) {
		if (plDynamicPredicates == null) {
			PrologSession session = null;
			try {
				PDTPlugin r = PDTPlugin.getDefault();
				session = plProject.getPrologInterface()
						.getSession();
				List solutions = session
						.queryAll("predicate_property(P,dynamic),functor(P,Name,_)");
				List keywords = new ArrayList();
				for (Iterator it = solutions.iterator(); it.hasNext();) {
					Map si = (Map) it.next();
					String name = (String) si.get("Name");
					keywords.add(name);
				}
				plDynamicPredicates = (String[]) keywords
						.toArray(new String[0]);
			} catch (Exception e) {
				plDynamicPredicates = new String[0];
				Debug.report(e);
			} finally {
				if (session != null)
					session.dispose();
			}
		}
	}

	private void initKeywords(IPrologProject plProject) {
		if (plKeywords == null) {
			PrologSession session = null;
			;
			try {
				PDTPlugin r = PDTPlugin.getDefault();
				session = plProject.getPrologInterface()
						.getSession();
				List solutions = session
						.queryAll("predicate_property(P,built_in),functor(P,Name,_)");
				List keywords = new ArrayList();
				for (Iterator it = solutions.iterator(); it.hasNext();) {
					Map si = (Map) it.next();
					String name = (String) si.get("Name");
					keywords.add(name);
				}

				plKeywords = (String[]) keywords.toArray(new String[0]);
			} catch (PrologException e) {
				plKeywords = new String[0];
				Debug.report(e);
			} finally {
				if (session != null) {
					session.dispose();
				}
			}
		}
	}

	public PLScanner(PLEditor editor, ColorManager manager) {
		IFileEditorInput editorInput = (IFileEditorInput) editor.getEditorInput();

		IPrologProject plProject;
		try {
			plProject = (IPrologProject) editorInput.getFile().getProject()
					.getNature(PDTCore.NATURE_ID);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}		

		initKeywords(plProject);
		initDynamicPredicates(plProject);
		IToken procInstr = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.PL_VAR), null, 1));
		IToken keywordToken = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.PL_KEYWORD), null, 1));

		IToken dynamicToken = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.PL_DYNAMIC), null, 1));
		IToken wordToken = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.PL_KEYWORD), null, 0));

		IToken string = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.STRING)));

		IRule[] rules = new IRule[5];
		// Add rule for processing instructions

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
		// rules[5] = dynamicRule;

		setRules(rules);
	}
}
