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

package org.cs3.pdt.internal.editors;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IFileEditorInput;

import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader.Array;

public class PLScanner extends RuleBasedScanner {

	public PLScanner(PLEditor editor, ColorManager manager) 
	    throws CoreException, PrologInterfaceException {
		IFileEditorInput editorInput = null;
		IProject project = null;
		IPrologProject plProject = null;
		IFile file;
		
		// TODO: Add treatment of error cases (missing else branches): 

		assert(editor!=null);
		
		if (editor.getEditorInput() instanceof IFileEditorInput) {
			editorInput = (IFileEditorInput) editor.getEditorInput();
		}
		assert (editorInput != null) ;
//		{
			file = editorInput.getFile();
		assert (file != null) ;
			project = file.getProject();
//		}
		if (project != null && project.exists() && project.hasNature(PDTCore.NATURE_ID)) {
			plProject = (IPrologProject) project.getNature(PDTCore.NATURE_ID);
		}
		
		// "Tokens" indicate the desired highlighting
		IToken variableToken    = tokenFor(PDTColors.VARIABLE,manager);
		IToken stringToken      = tokenFor(PDTColors.STRING,manager);
		IToken wordToken        = tokenFor(PDTColors.DEFAULT,manager);

        // Create rules for syntax highlighting of ...
		IRule[] rules = new IRule[5];		
        // - variables:
		rules[0] = new VarRule(variableToken);
		// - double quotes:
		rules[1] = new SingleLineRule("\"", "\"", stringToken, '\\');
		// - single quotes:
		rules[2] = new SingleLineRule("'", "'", stringToken, '\\');
		// - whitespace:
		rules[3] = new WhitespaceRule(new PLWhitespaceDetector());
		// - special words: 
		rules[4] = new WordRule(new WordDetector(), wordToken);
		addWordsTo((WordRule)rules[4], manager, file, plProject);

		// Activate the defined rules.
		setRules(rules);
	}

	/**
	 * Add to the special words of wordRule all names of predicates 
	 * that should be highlighted in a specific way and associate
	 * them to tokens that indicate the desired highlighting.
	 * 
     * @param wordRule -- The WordRule to which we add words.
	 * @param manager -- The ColorManager 
	 * @param plProject -- The PDT Metadata process.
	 * @throws PrologInterfaceException
	 */
	private void addWordsTo(WordRule wordRule, ColorManager manager, 
			IFile file, IPrologProject plProject) throws PrologInterfaceException {
		
		// The order of the following definitions is important!
		// The latter ones overrule the previous ones. E.g. a predicate
		// that is transparent AND a metapredicate will be highlighted
		// as a metapredicate because the metapredicates are added later.
		// This makes sense since being a metapredicate is more specific
		// (each metapredicate is transparent but not every transparent
		// predicate is a metapredicate). -- GK
		
		addWordsWithProperty("undefined", tokenFor(PDTColors.UNDEFINED,manager), wordRule, file, plProject);
		addWordsWithProperty("built_in", tokenFor(PDTColors.KEYWORD,manager), wordRule, file, plProject);
		addWordsWithProperty("dynamic",  tokenFor(PDTColors.DYNAMIC,manager), wordRule, file, plProject);
		addWordsWithProperty("transparent", tokenFor(PDTColors.TRANSPARENT,manager), wordRule, file, plProject);
		addWordsWithProperty("meta_predicate(_)", tokenFor(PDTColors.META,manager), wordRule, file, plProject);
	}


	private Token tokenFor(RGB color, ColorManager manager) {
		return new Token(new TextAttribute(manager.getColor(color), null, 1));
	}
	
	/**
	 * Add to the special words of wordRule all names of predicates 
	 * that have a certain property and associate each with the 
	 * token that indicates the desired highlighting.
	 * 
	 * @param property -- The desired property 
	 * @param keywordToken -- The desired highlighting for words with that property
	 * @param wordRule -- The WordRule to which to add the words.
	 * @param plProject -- The PDT Metadata process.
	 * @throws PrologInterfaceException
	 */
	private void addWordsWithProperty(String property, IToken keywordToken, WordRule wordRule,
			IFile file, IPrologProject plProject) throws PrologInterfaceException {
		String[] plBuiltInPredicates = getPredicatesWithProperty(property,file,plProject);
		for (int i = 0; plBuiltInPredicates!=null&&i < plBuiltInPredicates.length; i++){
			wordRule.addWord(plBuiltInPredicates[i], keywordToken);
		}
	}


	/* The two different implementations that are used here should be
	 * reconciled / integrated. I see no reason, why the current 
	 * NonPDT behaviour should not be applied also if the PDT nature 
	 * is active.  -- GK, April 2011
	 */
	private String[] getPredicatesWithProperty(String property,
			IFile file, IPrologProject plProject) throws PrologInterfaceException {
		if (plProject == null) // The project does NOT have the PDT nature
			return getPredicatesWithProperty__NonPDT(property, file);
		else
			return getPredicatesWithProperty__PDT(property, plProject);
	}

	/**
	 * getPredicatesWithProperty__NonPDT(String property)
	 * 
	 * Get names of predicates that have a certain property. This implementation
	 * (by Tobias Rho) is for projects that DO NOT have the PDT nature.
	 * 
	 * TODO: Integrate / reoncile the two versions.
	 * 
	 * @param plProject
	 */
	public String[] getPredicatesWithProperty__NonPDT(String property,IFile file) {
		PrologConsole console = PrologConsolePlugin.getDefault()
				.getPrologConsoleService().getActivePrologConsole();
		if (console == null) {
			return null;
		}
		PrologSession session = null;
		try {
			session = console.getPrologInterface().getSession();
			// long before=System.currentTimeMillis();
			Map<String, Object> solutions = session
					.queryOnce("predicates_with_property(" 
							+ property
							+ ",'" 
							+ file.getName()
							+ "',Predicates)");
			// System.out.println("Resolving dynamic predicates took: "
			// +(System.currentTimeMillis()-before));

			String predicatesStr = (String) solutions.get("Predicates");
			// swipl 5.8.x adds ", " between list elements when writing
			// Strings/Streams:
			predicatesStr = predicatesStr.replaceAll(" ", "");
			return predicatesStr.substring(1, predicatesStr.length() - 1)
					.split(",");
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null)
				session.dispose();
		}
		return null;
	}

	/**
	 * getPredicatesWithProperty__PDT(String property, IPrologProject plProject )
	 * 
	 * Get names of predicates that have a certain property. This implementation
	 * (by Lukas Degener) is for projects that have the PDT nature.
	 * 
	 * TODO: Integrate / reoncile the two versions.
	 * 
	 * @param plProject
	 */
	private String[] getPredicatesWithProperty__PDT(String property, 
			IPrologProject plProject) {
		PrologSession session = null;
		try {
			session = plProject.getMetadataPrologInterface().getSession(
					PrologInterface.NONE);
			List<Map<String, Object>> solutions = session
					.queryAll("predicate_property(M:P," + property
							+ "),functor(P,Name,_)"); // M:P also gets unused
														// pred's
			List<String> keywords = new ArrayList<String>();
			for (Iterator<Map<String, Object>> it = solutions.iterator(); it
					.hasNext();) {
				Map<String, Object> si = it.next();
				String name = (String) si.get("Name");
				keywords.add(name);
			}
			return keywords.toArray(new String[0]);
		} catch (Exception e) {
			Debug.report(e);
			return new String[0];
		} finally {
			if (session != null)
				session.dispose();
		}
	}
}
