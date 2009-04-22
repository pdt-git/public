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

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.actions.FindPredicateActionDelegate;
import org.cs3.pdt.internal.actions.ReferencesActionDelegate;
import org.cs3.pdt.internal.actions.SpyPointActionDelegate;
import org.cs3.pdt.internal.actions.ToggleCommentAction;
import org.cs3.pdt.internal.views.PrologOutline;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalData;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.ui.actions.IJavaEditorActionDefinitionIds;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

public class PLEditor extends TextEditor {

	public static String COMMAND_SHOW_TOOLTIP = "org.eclipse.pdt.ui.edit.text.prolog.show.prologdoc";

	public static String COMMAND_TOGGLE_COMMENTS = "org.eclipse.pdt.ui.edit.text.prolog.toggle.comments";

	private ColorManager colorManager;

	private PrologOutline fOutlinePage;

	protected final static char[] BRACKETS = { '(', ')', '[', ']' };

	private PLConfiguration configuration;

	private IProgressMonitor monitor = new NullProgressMonitor();

	private IContentAssistant assistant;

	protected void initializeKeyBindingScopes() {
		setKeyBindingScopes(new String[] { PDT.CONTEXT_EDITING_PROLOG_CODE });
	}

	public void doSave(IProgressMonitor progressMonitor) {
		super.doSave(progressMonitor);

	}

	protected abstract class AbstractSelectionChangedListener implements
			ISelectionChangedListener {

		/**
		 * Installs this selection changed listener with the given selection
		 * provider. If the selection provider is a post selection provider,
		 * post selection changed events are the preferred choice, otherwise
		 * normal selection changed events are requested.
		 * 
		 * @param selectionProvider
		 */
		public void install(ISelectionProvider selectionProvider) {
			if (selectionProvider == null)
				return;

			if (selectionProvider instanceof IPostSelectionProvider) {
				IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
				provider.addPostSelectionChangedListener(this);
			} else {
				selectionProvider.addSelectionChangedListener(this);
			}
		}

		/**
		 * Removes this selection changed listener from the given selection
		 * provider.
		 * 
		 * @param selectionProviderstyle
		 */
		public void uninstall(ISelectionProvider selectionProvider) {
			if (selectionProvider == null)
				return;

			if (selectionProvider instanceof IPostSelectionProvider) {
				IPostSelectionProvider provider = (IPostSelectionProvider) selectionProvider;
				provider.removePostSelectionChangedListener(this);
			} else {
				selectionProvider.removeSelectionChangedListener(this);
			}
		}
	}

	public PLEditor() {
		super();
		try {
			colorManager = new ColorManager();
			configuration = new PLConfiguration(colorManager, this);

			setSourceViewerConfiguration(configuration);
			setDocumentProvider(new PLDocumentProvider());
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	private static final String SEP_INSPECT = "inspect";

	private IFile file;

	

	private static final String MATCHING_BRACKETS = "matching.brackets";

	private static final String MATCHING_BRACKETS_COLOR = "matching.brackets.color";

	protected void configureSourceViewerDecorationSupport(
			SourceViewerDecorationSupport support) {
		getPreferenceStore().setDefault(MATCHING_BRACKETS, true);
		getPreferenceStore().setDefault(MATCHING_BRACKETS_COLOR, "30,30,200");
		support.setCharacterPairMatcher(new PLCharacterPairMatcher());
		support.setMatchingCharacterPainterPreferenceKeys(MATCHING_BRACKETS,
				MATCHING_BRACKETS_COLOR);

		super.configureSourceViewerDecorationSupport(support);
	}

	public void createPartControl(final Composite parent) {
		try {
			createPartControl_impl(parent);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t.getMessage(), t);
		}
	}

	private void createPartControl_impl(final Composite parent) {
		super.createPartControl(parent);

		MenuManager menuMgr = createPopupMenu();

		createInspectionMenu(menuMgr);

		assistant = configuration.getContentAssistant(getSourceViewer());
		Action action;

		// TODO: create own bundle
		ResourceBundle bundle = ResourceBundle.getBundle(PDT.RES_BUNDLE_UI);
		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ContextAssistProposal", this) {
			public void run() {
				assistant.showPossibleCompletions();
			}
		};
		addAction(menuMgr, action, "Context Assist Proposal",
				IWorkbenchActionConstants.MB_ADDITIONS,
				ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);

		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ToolTipAction", this) {
			public void run() {
				assistant.showContextInformation();
			}
		};
		addAction(menuMgr, action, "Show Tooltip",
				IWorkbenchActionConstants.MB_ADDITIONS, COMMAND_SHOW_TOOLTIP);

		ToggleCommentAction tca = new ToggleCommentAction(bundle,
				PLEditor.class.getName() + ".ToggleCommentsAction", this);
		tca.configure(getSourceViewer(), configuration);
		tca.setEnabled(true);
		addAction(menuMgr, tca, "Toggle Comments",
				IWorkbenchActionConstants.MB_ADDITIONS, COMMAND_TOGGLE_COMMENTS);

	}

	/**
	 * @param menuMgr
	 */
	private void createInspectionMenu(MenuManager menuMgr) {
		addAction(menuMgr, new FindPredicateActionDelegate(this),
				"Open Declaration", SEP_INSPECT,
				IJavaEditorActionDefinitionIds.OPEN_EDITOR);

		addAction(menuMgr, new SpyPointActionDelegate(this),
				"Toggle Spy Point", SEP_INSPECT,
				"org.eclipse.debug.ui.commands.ToggleBreakpoint");

		addAction(menuMgr, new ReferencesActionDelegate(this),
				"Find References", SEP_INSPECT,
				IJavaEditorActionDefinitionIds.SEARCH_REFERENCES_IN_WORKSPACE);

	}

	/**
	 * @param menuMgr
	 */
	private void addAction(MenuManager menuMgr, Action action, String name,
			String separator, String id) {

		action.setActionDefinitionId(id);
		action.setText(name);
		menuMgr.appendToGroup(separator, action);
		setAction(
				IJavaEditorActionDefinitionIds.SEARCH_REFERENCES_IN_WORKSPACE,
				action);
	}

	/**
	 * @return
	 */
	private MenuManager createPopupMenu() {
		MenuManager menuMgr = new MenuManager(
				"org.cs3.pl.editors.PLEditor", "org.cs3.pl.editors.PLEditor"); //$NON-NLS-1$
		menuMgr.addMenuListener(getContextMenuListener());
		menuMgr.add(new Separator(SEP_INSPECT));
		menuMgr.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		getSourceViewer().getTextWidget().setMenu(
				menuMgr.createContextMenu(getSourceViewer().getTextWidget()));
		getEditorSite().registerContextMenu(menuMgr, getSelectionProvider());
		return menuMgr;
	}

	/**
	 * @param parent
	 */

	public Object getAdapter(Class required) {
		try {
			if (IContentOutlinePage.class.equals(required)) {
				if (fOutlinePage == null) {
					// fOutlinePage= new PrologOutline(this,
					// ((IFileEditorInput)getEditorInput()).getFile());
					fOutlinePage = new PrologOutline(this);
					fOutlinePage.setInput(getEditorInput());
				}
				return fOutlinePage;
			}
			// if (required.equals(IPropertySheetPage.class)) {
			// return new PropertySheetPage();
			// }
			return super.getAdapter(required);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	/**
	 * Informs the editor that its outliner has been closed.
	 */
	public void outlinePageClosed() {
		if (fOutlinePage != null) {

			fOutlinePage = null;
			resetHighlightRange();
		}
	}

	public PrologOutline getOutlinePage() {
		return fOutlinePage;
	}

	/**
	 * @param i
	 */
	public void gotoLine(int line) {
		Document document;
		document = (Document) getDocumentProvider().getDocument(
				getEditorInput());
		// System.out.println("namen: "+getEditorInput().getName());
		int offset;
		try {
			offset = document.getLineInformation(line - 1).getOffset();
			TextSelection newSelection = new TextSelection(document, offset, 0);
			getEditorSite().getSelectionProvider().setSelection(newSelection);
		} catch (BadLocationException e) {
			Debug.report(e);
		}
	}

	/**
	 * @param i
	 */
	public void gotoOffset(int offset) {
		Document document;
		document = (Document) getDocumentProvider().getDocument(
				getEditorInput());
		TextSelection newSelection = new TextSelection(document, offset, 0);
		getEditorSite().getSelectionProvider().setSelection(newSelection);
	}

	/**
	 * @return
	 */
	public String getSelectedLine() {

		Document document = (Document) getDocumentProvider().getDocument(
				getEditorInput());

		String line = null;
		try {
			TextSelection selection = (TextSelection) getEditorSite()
					.getSelectionProvider().getSelection();
			IRegion info = document.getLineInformationOfOffset(selection
					.getOffset());
			int start = findEndOfWhiteSpace(document, info.getOffset(), info
					.getOffset()
					+ info.getLength());

			line = document.get(start, info.getLength() + info.getOffset()
					- start);
		} catch (BadLocationException e) {
			Debug.report(e);

		}
		return line;
	}

	public static int findEndOfWhiteSpace(IDocument document, int offset,
			int end) throws BadLocationException {
		while (offset < end) {
			char c = document.getChar(offset);
			if (c != ' ' && c != '\t') {
				return offset;
			}
			offset++;
		}
		return end;
	}

	/**
	 * @return
	 */
	public Goal getSelectedPrologElement() throws BadLocationException {
		Document document = (Document) getDocumentProvider().getDocument(
				getEditorInput());

		String line = null;
		TextSelection selection = (TextSelection) getEditorSite()
				.getSelectionProvider().getSelection();
		int offset = selection.getOffset();

		return getPrologDataFromOffset(Util.prologFileName(file.getLocation()
				.toFile()), document, offset);
	}

	/**
	 * @param document
	 * @param offset
	 * @return
	 */
	public static Goal getPrologDataFromOffset(String file, IDocument document,
			int offset) throws BadLocationException {

		int start = offset;
		int end = offset;
		while (isPredicatenameChar(document.getChar(start)) && start > 0) {
			start--; // scan left until first non-predicate char
		}
		start++; // start is now the position of the first predictate char
		// (or module prefix char)

		while (isPredicatenameChar(document.getChar(end))
				&& end < document.getLength()) {
			end++;// scan right for first non-predicate char
		}

		if (start > end) {
			return null;
		}

		// element name will be the functor name, prefixed with a module name,
		// if present.
		String elementName = document.get(start, end - start);

		int endOfWhiteSpace = findEndOfWhiteSpace(document, end, document
				.getLength());
		int arity = 1;
		if (document.getLength() > endOfWhiteSpace
				&& document.getChar(endOfWhiteSpace) == '/') {
			end = endOfWhiteSpace + 1;
			String buf = "";
			while (end < document.getLength() && document.getChar(end) >= '0') {
				buf += document.getChar(end);
				end++;
			}
			if (buf.length() == 0)
				return null;
			arity = Integer.parseInt(buf);
			return new GoalData(file, null, elementName, arity);

		}
		if (document.getLength() == endOfWhiteSpace
				|| document.getChar(endOfWhiteSpace) != '(') {

			if (elementName.endsWith(":")) {
				return new GoalData(file,null, elementName.substring(0, elementName
						.length() - 1), -1);
			}
			String[] fragments = elementName.split(":");
			if (fragments.length == 2) {
				return new GoalData(file,fragments[0], fragments[1], 0);
			}
			String module = null;

			return new GoalData(file,module, elementName, 0);
		}

		end = endOfWhiteSpace + 1;
		whileLoop: while (!isClauseEnd(document, end)) {
			char c = document.getChar(end);
			switch (c) {
			case '(':
				end = consume(document, end + 1, ')');
				break;
			case '[':
				end = consume(document, end + 1, ']');
				break;
			case '"':
				end = consumeString(document, end + 1, '"');
				break;
			case '\'':
				end = consumeString(document, end + 1, '\'');
				break;
			case ',':
				arity++;
				break;
			case ')':
				break whileLoop;
			default:
				end++;
			}

			if (c == ',')

				end++;
		}

		String[] fragments = elementName.split(":");
		if (fragments.length == 2) {
			return new GoalData(file,fragments[0], fragments[1], arity);
		}
		String module = null;

		return new GoalData(file,module, elementName, arity);

	}

	/**
	 * @param document
	 * @param end
	 * @param c
	 * @return
	 */
	public static int consume(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			case '(':
				end = consume(document, end + 1, ')');
				break;
			case '[':
				end = consume(document, end + 1, ']');
				break;
			case '"':
				end = consumeString(document, end + 1, '"');
				break;
			case '\'':
				end = consumeString(document, end + 1, '\'');
				break;
			default:
				end++;
			}
		}
		return end + 1;
	}

	/**
	 * @param document
	 * @param i
	 * @param c
	 * @return
	 */
	private static int consumeString(IDocument document, int end, char endChar)
			throws BadLocationException {
		while (!(document.getChar(end) == endChar)) {
			char c = document.getChar(end);
			switch (c) {
			case '\\':
				end = end + 2;
				break;
			case '"':
				throw new RuntimeException(
						"This point should never been reached");
			default:
				end++;
			}
		}
		return end + 1;
	}

	/**
	 * @param c
	 * @return
	 */
	public static boolean isClauseEnd(IDocument document, int i) {
		try {
			if (document.getChar(i) == '.')
				return true;
			if (document.getChar(i) == ':' && document.getLength() > (i + 1)
					&& document.getChar(i + 1) == '-')
				return true;
		} catch (BadLocationException e) {
			Debug.report(e);
		}
		return false;
	}

	static public boolean isFunctorChar(char c) {
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c == '_')
			return true;
		// if (c == '-')
		// return false;

		return false;
	}

	/**
	 * @param c
	 * @return
	 */
	static public boolean isPredicatenameChar(char c) {
		if (c >= 'a' && c <= 'z')
			return true;
		if (c >= '0' && c <= '9')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c == ':' || c == '_' || c == '+' || c == '-' || c == '\\'
				|| c == '*')
			return true;
		return false;
	}

	static public boolean isNonQualifiedPredicatenameChar(char c) {
		return isPredicatenameChar(c) && c != ':';
	}

	public TextSelection getSelection() {
		Document document = (Document) getDocumentProvider().getDocument(
				getEditorInput());
		Display display = PDTPlugin.getDefault().getWorkbench().getDisplay();

		String line = null;
		return (TextSelection) getEditorSite().getSelectionProvider()
				.getSelection();
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarPrefix(String prefix) {
		if (prefix.length() == 0)
			return false;
		return isVarPrefix(prefix.charAt(0));
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarPrefix(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isVarChar(char c) {
		if (c == '_')
			return true;
		if (c >= 'A' && c <= 'Z')
			return true;
		if (c >= 'a' && c <= 'z')
			return true;
		return false;
	}

	/**
	 * @param document
	 * @param l
	 * @return
	 */
	public static boolean predicateDelimiter(IDocument document, int l)
			throws BadLocationException {
		char c = document.getChar(l);
		if (c == '.') {
			if (l > 0 && document.getChar(l - 1) == '.')
				return false;
			if (l < document.getLength() - 1 && document.getChar(l + 1) == '.')
				return false;
			return true;
		}
		return false;
	}

	/**
	 * @param prefix
	 * @return
	 */
	public static boolean isFunctorPrefix(String prefix) {
		if (prefix == null | prefix.length() == 0)
			return false;
		if (prefix.charAt(0) >= 'a' && prefix.charAt(0) <= 'z')
			return true;

		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.editors.text.TextEditor#doSetInput(org.eclipse.ui.IEditorInput)
	 */
	protected void doSetInput(IEditorInput input) throws CoreException {
		checkForPrologNature(input);
		if (fOutlinePage != null) {
			fOutlinePage.setInput(input);
		}
		super.doSetInput(input);
		IFileEditorInput editorInput = null;
		if (input instanceof IFileEditorInput) {
			editorInput = (IFileEditorInput) input;

		}
		if (editorInput == null) {
			return;
		}
		file=editorInput.getFile();
	}

	private void checkForPrologNature(IEditorInput input) {
		IFileEditorInput editorInput = null;
		if (input instanceof IFileEditorInput) {
			editorInput = (IFileEditorInput) input;

		}
		if (editorInput == null) {
			return;
		}
		IProject project = editorInput.getFile().getProject();
		IPrologProject plProject = null;

		try {
			if (project.hasNature(PDTCore.NATURE_ID)) {
				plProject = (IPrologProject) editorInput.getFile().getProject()
						.getNature(PDTCore.NATURE_ID);
			}
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		if (plProject == null) {
			String dialogTitle = "Add Prolog Nature?";
			String dialogMessage = "The project "
					+ project.getName()
					+ " is not a Prolog Project. Do you wish to make it a Prolog Project?";
			String toggleMessage = null;
			PDTPlugin plugin = PDTPlugin.getDefault();
			String key = PDT.PREF_ADD_NATURE_ON_OPEN;
			String pref = plugin.getPreferenceValue(key,
					MessageDialogWithToggle.PROMPT);
			boolean toggleState = false;
			boolean shouldAddNature = MessageDialogWithToggle.ALWAYS
					.equals(pref);
			IPreferenceStore store = plugin.getPreferenceStore();
			if (MessageDialogWithToggle.PROMPT.equals(pref)) {
				MessageDialogWithToggle toggle = MessageDialogWithToggle
						.openYesNoQuestion(getEditorSite().getShell(),
								dialogTitle, dialogMessage, toggleMessage,
								toggleState, store, key);
				shouldAddNature = IDialogConstants.YES_ID == toggle
						.getReturnCode();
			}
			if (shouldAddNature) {
				try {
					PDTCoreUtils.addPDTNature(project);
				} catch (CoreException e) {
					Debug.report(e);
					throw new RuntimeException(e);
				}
			}
		}
	}

}