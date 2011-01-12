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
import org.eclipse.ui.IFileEditorInput;

import com.sun.xml.internal.bind.v2.runtime.unmarshaller.XsiNilLoader.Array;

public class PLScanner extends RuleBasedScanner {
	// public static final String[] plKeywords = {"assert", "retract",
	// "print","write"};
	public static String[] plKeywords = null;

	// {"assert", "retract", "print","write","functor",
	// "atom_concat","explain","retractall","nth_clause","atom_to_term","atom_to_term",
	// "number", "bagof","findall"};
	private String[] plDynamicPredicates = null;

	private void initDynamicPredicates(IPrologProject plProject) {
//		if(plDynamicPredicates==null){
//			return;
//		}
		if(plProject==null){
			plDynamicPredicates= getPredicatesWithProperty("dynamic");
			return;
		}
		if (plDynamicPredicates == null) {
			
			PrologSession session = null;
			try {
				session = plProject.getMetadataPrologInterface().getSession(PrologInterface.NONE);
				List<Map<String,Object>> solutions = session.queryAll("predicate_property(M:P,dynamic),functor(P,Name,_)");  // M:P is a prolog-trick to get also unused pred's
				List<String> keywords = new ArrayList<String>();
				for (Iterator<Map<String,Object>> it = solutions.iterator(); it.hasNext();) {
					Map<String,Object> si = it.next();
					String name = (String) si.get("Name");
					keywords.add(name);
				}
				plDynamicPredicates = keywords
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

	private void initKeywords(IPrologProject plProject) throws PrologInterfaceException {
		//plKeywords=null;
		if(plKeywords!=null)
			return;
		if(plProject==null){
			plKeywords = getPredicatesWithProperty("built_in");
			return;
			
//			plKeywords = new String[]{"dynamic","multifile","module","use_module"};

		}
		if (plKeywords == null) {
			
			PrologSession session = null;
			;
			try {
				session = plProject.getMetadataPrologInterface().getSession(PrologInterface.NONE);
				List<Map<String,Object>> solutions = session
						.queryAll("predicate_property(M:P,built_in),functor(P,Name,_)"); // M:P is a prolog-trick to get also unused pred's
				List<String> keywords = new ArrayList<String>();
				for (Iterator<Map<String,Object>> it = solutions.iterator(); it.hasNext();) {
					Map<String,Object> si = it.next();
					String name = (String) si.get("Name");
					keywords.add(name);
				}
				plKeywords = keywords.toArray(new String[0]);
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

	public String[] getPredicatesWithProperty(String property) {
		PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		if(console==null){
			return null;
		}
		PrologSession session=null;
		try {
			session = console.getPrologInterface().getSession();
			// long before=System.currentTimeMillis();
			Map<String, Object> solutions=session.queryOnce("predicates_with_property("+property+",Predicates)");
			//System.out.println("Resolving dynamic predicates took: " +(System.currentTimeMillis()-before));
			
			String predicatesStr = (String)solutions.get("Predicates");
			// swipl 5.8.x adds ", " between list elements when writing Strings/Streams: 
			predicatesStr=predicatesStr.replaceAll(" ", "");
			return predicatesStr.substring(1, predicatesStr.length()-1).split(",");
		}catch(Exception e){
			Debug.report(e);
		} finally {
			if(session!=null)session.dispose();
		}
		return null;
	}

	public PLScanner(PLEditor editor, ColorManager manager) throws CoreException, PrologInterfaceException {
		IFileEditorInput editorInput = null;
		IProject project = null;
		IPrologProject plProject = null;
		if (editor.getEditorInput() instanceof IFileEditorInput) {
			editorInput = (IFileEditorInput) editor.getEditorInput();
		}
		if (editorInput != null) {
			project = editorInput.getFile().getProject();
		}
		
			if (project != null && project.exists() && project.hasNature(PDTCore.NATURE_ID)) {
				plProject = (IPrologProject) project
						.getNature(PDTCore.NATURE_ID);
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
				.getColor(IPLColorConstants.DEFAULT), null, 0));

		IToken string = new Token(new TextAttribute(manager
				.getColor(IPLColorConstants.STRING)));

		IRule[] rules = new IRule[5];
		// Add rule for processing instructions

		WordRule wordRule = new WordRule(new WordDetector(), wordToken);

		for (int i = 0; plKeywords!=null&&i < plKeywords.length; i++){
			wordRule.addWord(plKeywords[i], keywordToken);
		}
		for (int i = 0; plDynamicPredicates!=null&&i < plDynamicPredicates.length; i++){
			wordRule.addWord(plDynamicPredicates[i], dynamicToken);
		}

		rules[0] = new VarRule(procInstr);
		// Add rule for double quotes
		rules[1] = new SingleLineRule("\"", "\"", string, '\\');
		// Add a rule for single quotes
		rules[2] = new SingleLineRule("'", "'", string, '\\');
		// Add generic whitespace rule.
		rules[3] = new WhitespaceRule(new PLWhitespaceDetector());
		rules[4] = wordRule;

		setRules(rules);
	}
}
