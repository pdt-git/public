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

import java.io.IOException;
import java.net.URI;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.internal.resources.File;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.rules.IRule;
import org.eclipse.jface.text.rules.IToken;
import org.eclipse.jface.text.rules.RuleBasedScanner;
import org.eclipse.jface.text.rules.SingleLineRule;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.rules.WhitespaceRule;
import org.eclipse.jface.text.rules.WordRule;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;

public class PLScanner extends RuleBasedScanner implements IPropertyChangeListener{
	private ColorManager manager;
	private IFile file;

	public PLScanner(PLEditor editor, ColorManager manager) 
	throws CoreException, PrologInterfaceException {

		this.manager = manager;
		
		assert(editor!=null);
		
		IEditorInput input = editor.getEditorInput();

		if (input instanceof IFileEditorInput) {
			IFileEditorInput editorInput = (IFileEditorInput) input;
			if (editorInput != null) {
				file = editorInput.getFile();
			}
		}
		else if (input instanceof FileStoreEditorInput) {
			FileStoreEditorInput storeEditorInput = (FileStoreEditorInput) input;
			if (storeEditorInput != null) {
				URI uri = storeEditorInput.getURI();
				String path = uri.getPath();
				try {
					file = PDTCoreUtils.findFileForLocation(path);
				} catch (IOException e) {

				}
			}
		}
		
		IPreferenceStore store = PDTPlugin.getDefault().getPreferenceStore();
		store.addPropertyChangeListener(this);
		
		initHighlighting();
	}

	private void initHighlighting()
			throws PrologInterfaceException, CoreException {
		// "Tokens" indicate the desired highlighting
		IToken variableToken    = tokenFor(manager.getVariableColor());
		IToken stringToken      = tokenFor(manager.getStringColor());
		IToken wordToken        = tokenFor(manager.getDefaultColor());

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
		addWordsTo((WordRule)rules[4]);

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
	private void addWordsTo(WordRule wordRule) throws PrologInterfaceException, CoreException {
		
		// The order of the following definitions is important!
		// The latter ones overrule the previous ones. E.g. a predicate
		// that is transparent AND a metapredicate will be highlighted
		// as a metapredicate because the metapredicates are added later.
		// This makes sense since being a metapredicate is more specific
		// (each metapredicate is transparent but not every transparent
		// predicate is a metapredicate). -- GK
		
		addWordsWithProperty("undefined", tokenFor(manager.getUndefinedColor()), wordRule);
		addWordsWithProperty("built_in", tokenFor(manager.getKeywordColor()), wordRule);
		addWordsWithProperty("dynamic",  tokenFor(manager.getDynamicColor()), wordRule);
		addWordsWithProperty("transparent", tokenFor(manager.getTransparentColor()), wordRule);
		addWordsWithProperty("meta_predicate(_)", tokenFor(manager.getMetaColor()), wordRule);
	}


	private Token tokenFor(RGB color) {
		return new Token(new TextAttribute(manager.getColor(color), null, 1)); /*SWT.NORMAL | SWT.ITALIC | SWT.BOLD | TextAttribute.UNDERLINE)*/
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
	 * @throws CoreException 
	 */
	private void addWordsWithProperty(String property, IToken keywordToken, WordRule wordRule) throws PrologInterfaceException, CoreException {
		String[] plBuiltInPredicates = getPredicatesWithProperty(property);
		for (int i = 0; plBuiltInPredicates!=null&&i < plBuiltInPredicates.length; i++){
			wordRule.addWord(plBuiltInPredicates[i], keywordToken);
		}
	}


	/* The two different implementations that are used here should be
	 * reconciled / integrated. I see no reason, why the current 
	 * NonPDT behaviour should not be applied also if the PDT nature 
	 * is active.  -- GK, April 2011
	 */
	private String[] getPredicatesWithProperty(String property) throws PrologInterfaceException, CoreException {
		IPrologProject plProject = getPLProjectForFile(file);
		
		if (plProject == null) // The project does NOT have the PDT nature
			return getPredicatesWithProperty__NonPDT(property);
		else
			return getPredicatesWithProperty__PDT(property);
	}

	public IPrologProject getPLProjectForFile(IFile file) throws CoreException {
		IProject project = file.getProject();
		if (project != null && project.exists() && project.hasNature(PDTCore.NATURE_ID)) {
			return (IPrologProject) project.getNature(PDTCore.NATURE_ID);
		};
		return null;
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
	public String[] getPredicatesWithProperty__NonPDT(String property) {
		PrologConsole console = PrologConsolePlugin.getDefault()
				.getPrologConsoleService().getActivePrologConsole();
		if (console == null) {
			return null;
		}
		PrologSession session = null;
		try {
			session = console.getPrologInterface().getSession();
			Map<String, Object> solutions = session
					.queryOnce("predicates_with_property(" 
							+ property
							+ ",'" 
							+ file.getName()
							+ "',Predicates)");

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
	private String[] getPredicatesWithProperty__PDT(String property) {
		PrologSession session = null;
		try {
			IPrologProject plProject = getPLProjectForFile(file);
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

	@Override
	public void propertyChange(PropertyChangeEvent event) {
		try {
			initHighlighting();
		} catch (CoreException e) {

		}
		catch (PrologInterfaceException e) {

		}	
	}
}
