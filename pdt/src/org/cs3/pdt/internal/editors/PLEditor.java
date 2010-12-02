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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
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
import org.cs3.pdt.internal.contentassistant.VariableCompletionProposal;
import org.cs3.pdt.internal.editors.PLEditor.OccurrenceLocation;
import org.cs3.pdt.internal.views.PrologOutline;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalData;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
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
import org.eclipse.jface.text.ISelectionValidator;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.custom.CaretEvent;
import org.eclipse.swt.custom.CaretListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
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

	private IContentAssistant assistant;

	@Override
	protected void initializeKeyBindingScopes() {
		setKeyBindingScopes(new String[] { PDT.CONTEXT_EDITING_PROLOG_CODE });
	}

	@Override
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
			
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	private static final String SEP_INSPECT = "inspect";

	private IPath filepath;

	

	private static final String MATCHING_BRACKETS = "matching.brackets";

	private static final String MATCHING_BRACKETS_COLOR = "matching.brackets.color";

	@Override
	protected void configureSourceViewerDecorationSupport(
			SourceViewerDecorationSupport support) {
		getPreferenceStore().setDefault(MATCHING_BRACKETS, true);
		getPreferenceStore().setDefault(MATCHING_BRACKETS_COLOR, "30,30,200");
		support.setCharacterPairMatcher(new PLCharacterPairMatcher());
		support.setMatchingCharacterPainterPreferenceKeys(MATCHING_BRACKETS,
				MATCHING_BRACKETS_COLOR);

		super.configureSourceViewerDecorationSupport(support);
	}

	@Override
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
			@Override
			public void run() {
				assistant.showPossibleCompletions();
			}
		};
		addAction(menuMgr, action, "Context Assist Proposal",
				IWorkbenchActionConstants.MB_ADDITIONS,
				ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);

		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ToolTipAction", this) {
			@Override
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
		
		
		getSourceViewer().getTextWidget().addCaretListener(new CaretListener() {
			
			@Override
			public void caretMoved(CaretEvent event) {
				updateOccurrenceAnnotations(event.caretOffset);
				
			}
		});

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

	@SuppressWarnings("rawtypes")
	@Override
	public Object getAdapter(Class required) {
		try {
			if (IContentOutlinePage.class.equals(required)) {
				if (fOutlinePage == null) {
					fOutlinePage = new PrologOutline(this);
					fOutlinePage.setInput(getEditorInput());
				}
				return fOutlinePage;
			}
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

		TextSelection selection = (TextSelection) getEditorSite()
				.getSelectionProvider().getSelection();
		int offset = selection.getOffset();

		return getPrologDataFromOffset(Util.prologFileName(filepath.toFile()), document, offset);
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
	@Override
	protected void doSetInput(IEditorInput input) throws CoreException {
		
		setDocumentProvider(createDocumentProvider(input));
		
		checkForPrologNature(input);
		if (fOutlinePage != null) {
			fOutlinePage.setInput(input);
		}
		super.doSetInput(input);
	
		if (input instanceof IFileEditorInput) {
			IFileEditorInput editorInput = (IFileEditorInput) input;
			filepath = editorInput.getFile().getLocation();
			
		}
		if (input instanceof FileStoreEditorInput){
			FileStoreEditorInput editorInput = (FileStoreEditorInput) input;
			filepath =  new Path(editorInput.getURI().getPath());
		}
		else{
			return;
		}
		
	}

	private IDocumentProvider createDocumentProvider(IEditorInput input) {
		 if(input instanceof FileStoreEditorInput){
             return new ExternalDocumentProvider();
		 } else{
			 return new PLDocumentProvider();
		 }
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
	private Object annotationModelMonitor= new Object();

	public Annotation[] fOccurrenceAnnotations;

	private OccurrencesFinderJob fOccurrencesFinderJob;

	private boolean fMarkOccurrenceAnnotations = true;

	private TextSelection oldSelection;

	class OccurrencesFinderJob extends Job {

		private final IDocument fDocument;
//		private final ISelectionValidator fPostSelectionValidator;
		private boolean fCanceled= false;
		private final OccurrenceLocation[] fLocations;

		public OccurrencesFinderJob(IDocument document, OccurrenceLocation[] locations) {
			super("update occurrences");
			fDocument= document;
			fLocations= locations;

//			if (getSelectionProvider() instanceof ISelectionValidator)
//				fPostSelectionValidator= (ISelectionValidator)getSelectionProvider();
//			else
//				fPostSelectionValidator= null;
		}

		// cannot use cancel() because it is declared final
		void doCancel() {
			fCanceled= true;
			cancel();
		}

		private boolean isCanceled(IProgressMonitor progressMonitor) {
			return fCanceled || progressMonitor.isCanceled()
//				||  fPostSelectionValidator != null && !(fPostSelectionValidator.isValid(fSelection) 
			 // TRHO || fForcedMarkOccurrencesSelection == fSelection
						
				|| LinkedModeModel.hasInstalledModel(fDocument);
		}

		/*
		 * @see Job#run(org.eclipse.core.runtime.IProgressMonitor)
		 */
		public IStatus run(IProgressMonitor progressMonitor) {
			if (isCanceled(progressMonitor))
				return Status.CANCEL_STATUS;

			ITextViewer textViewer= getSourceViewer();
			if (textViewer == null)
				return Status.CANCEL_STATUS;

			IDocument document= textViewer.getDocument();
			if (document == null)
				return Status.CANCEL_STATUS;

			IDocumentProvider documentProvider= getDocumentProvider();
			if (documentProvider == null)
				return Status.CANCEL_STATUS;

			IAnnotationModel annotationModel= documentProvider.getAnnotationModel(getEditorInput());
			if (annotationModel == null)
				return Status.CANCEL_STATUS;

			// Add occurrence annotations
			int length= fLocations.length;
			Map annotationMap= new HashMap(length);
			for (int i= 0; i < length; i++) {

				if (isCanceled(progressMonitor))
					return Status.CANCEL_STATUS;

				OccurrenceLocation location= fLocations[i];
				Position position= new Position(location.getOffset(), location.getLength());

				String description= location.getDescription();
				String annotationType= (location.getFlags() == 1) ? "org.eclipse.jdt.ui.occurrences.write" : "org.eclipse.jdt.ui.occurrences"; //$NON-NLS-1$ //$NON-NLS-2$

				annotationMap.put(new Annotation(annotationType, false, description), position);
			}

			if (isCanceled(progressMonitor))
				return Status.CANCEL_STATUS;

			synchronized (annotationModelMonitor) {
				if (annotationModel instanceof IAnnotationModelExtension) {
					((IAnnotationModelExtension)annotationModel).replaceAnnotations(fOccurrenceAnnotations, annotationMap);
				} else {
					removeOccurrenceAnnotations();
					Iterator iter= annotationMap.entrySet().iterator();
					while (iter.hasNext()) {
						Map.Entry mapEntry= (Map.Entry)iter.next();
						annotationModel.addAnnotation((Annotation)mapEntry.getKey(), (Position)mapEntry.getValue());
					}
				}
				fOccurrenceAnnotations= (Annotation[])annotationMap.keySet().toArray(new Annotation[annotationMap.keySet().size()]);
			}

			return Status.OK_STATUS;
		}
	}
	
	void removeOccurrenceAnnotations() {
//		fMarkOccurrenceModificationStamp= IDocumentExtension4.UNKNOWN_MODIFICATION_STAMP;
//		fMarkOccurrenceTargetRegion= null;

		IDocumentProvider documentProvider= getDocumentProvider();
		if (documentProvider == null)
			return;

		IAnnotationModel annotationModel= documentProvider.getAnnotationModel(getEditorInput());
		if (annotationModel == null || fOccurrenceAnnotations == null)
			return;

		synchronized (annotationModelMonitor) {
			if (annotationModel instanceof IAnnotationModelExtension) {
				((IAnnotationModelExtension)annotationModel).replaceAnnotations(fOccurrenceAnnotations, null);
			} else {
				for (int i= 0, length= fOccurrenceAnnotations.length; i < length; i++)
					annotationModel.removeAnnotation(fOccurrenceAnnotations[i]);
			}
			fOccurrenceAnnotations= null;
		}
	}
	
	protected void updateOccurrenceAnnotations(int caretOffset) {

		if (fOccurrencesFinderJob != null)
			fOccurrencesFinderJob.cancel();

		if (!fMarkOccurrenceAnnotations )
			return;

//		if (astRoot == null || selection == null)
//			return;

		IDocument document= getSourceViewer().getDocument();
		if (document == null)
			return;

//		if (document instanceof IDocumentExtension4) {
//			int offset= selection.getOffset();
//			long currentModificationStamp= ((IDocumentExtension4)document).getModificationStamp();
//			IRegion markOccurrenceTargetRegion= fMarkOccurrenceTargetRegion;
//			hasChanged= currentModificationStamp != fMarkOccurrenceModificationStamp;
//			if (markOccurrenceTargetRegion != null && !hasChanged) {
//				if (markOccurrenceTargetRegion.getOffset() <= offset && offset <= markOccurrenceTargetRegion.getOffset() + markOccurrenceTargetRegion.getLength())
//					return;
//			}
//			fMarkOccurrenceTargetRegion= JavaWordFinder.findWord(document, offset);
//			fMarkOccurrenceModificationStamp= currentModificationStamp;
//		}

		OccurrenceLocation[] locations= null;

//		ASTNode selectedNode= NodeFinder.perform(astRoot, selection.getOffset(), selection.getLength());
//		if (fMarkExceptions) {
//			ExceptionOccurrencesFinder finder= new ExceptionOccurrencesFinder();
//			if (finder.initialize(astRoot, selectedNode) == null) {
//				locations= finder.getOccurrences();
//			}
//		}
//
//		if (locations == null && fMarkMethodExitPoints) {
//			MethodExitsFinder finder= new MethodExitsFinder();
//			if (finder.initialize(astRoot, selectedNode) == null) {
//				locations= finder.getOccurrences();
//			}
//		}
//
//		if (locations == null && fMarkBreakContinueTargets) {
//			BreakContinueTargetFinder finder= new BreakContinueTargetFinder();
//			if (finder.initialize(astRoot, selectedNode) == null) {
//				locations= finder.getOccurrences();
//			}
//		}
//
//		if (locations == null && fMarkImplementors) {
//			ImplementOccurrencesFinder finder= new ImplementOccurrencesFinder();
//			if (finder.initialize(astRoot, selectedNode) == null) {
//				locations= finder.getOccurrences();
//			}
//		}
//
//		if (locations == null && selectedNode instanceof Name) {
//			IBinding binding= ((Name)selectedNode).resolveBinding();
//			if (binding != null && markOccurrencesOfType(binding)) {
//				OccurrencesFinder finder= new OccurrencesFinder();
//				if (finder.initialize(astRoot, selectedNode) == null) {
//					locations= finder.getOccurrences();
//				}
//			}
//		}
		ArrayList<OccurrenceLocation> locationList = new ArrayList<OccurrenceLocation>();
		TextSelection var=null;
		try {
			var = getVariableAtOffset(getDocumentProvider().getDocument(getEditorInput()),caretOffset);
			String varName = var.getText();
			if(oldSelection!=null && oldSelection.equals(var)){
				return;
			} else {
				oldSelection = var;
			}
			int begin=var.getOffset();
			if (PLEditor.isVarPrefix(varName) || varName.length() == 0) {
				boolean inAtom=false;
				int l = begin == 0 ? begin : begin - 1;
				String proposal = null;
				while (l > 0) {
					if(inAtom) {
						if(document.getChar(l)=='\''){						
							if(l == 0 || document.getChar(l-1)!='\''){
								inAtom=false;
							}
						}
					} else if(PLEditor.predicateDelimiter(document, l)){
						break;
					} else if(document.getChar(l)=='\''){
						inAtom=true;
					}					
					ITypedRegion region = document.getPartition(l);
					if (isComment(region))
						l = region.getOffset();
					else {
						char c = document.getChar(l);
						if (PLEditor.isVarChar(c)) {
							if (proposal == null)
								proposal = "";
							proposal = c + proposal;
						} else if (proposal != null) {
								if(var.getText().equals(proposal)) {
									locationList.add(new OccurrenceLocation(l+1, var.getLength(), 0,"desc"));
								}
							proposal = null;
						}
					}
					l--;
				}
			}
			if (PLEditor.isVarPrefix(varName) || varName.length() == 0) {
				int l = begin == document.getLength() ? begin : begin + 1;
				String proposal = null;
				boolean inAtom=false;
				while (l < document.getLength()) {
					if(inAtom) {
						if(document.getChar(l)=='\''){						
							if(l+1 == document.getLength() || document.getChar(l+1)!='\''){
								inAtom=false;
							}
						}
					} else if(PLEditor.predicateDelimiter(document, l)){
						break;
					} else if(document.getChar(l)=='\''){
						inAtom=true;
					}
					ITypedRegion region = document.getPartition(l);
					if (isComment(region)) {
						l = region.getOffset() + region.getLength();
					} else {
						char c = document.getChar(l);
						if (PLEditor.isVarChar(c)) {
							if (proposal == null)
								proposal = "";
							proposal = proposal + c;
						} else if (proposal != null) {
							if(var.getText().equals(proposal)) {
								locationList.add(new OccurrenceLocation(l-var.getLength(), var.getLength(), 0,"desc"));
							}
							proposal = null;
						}
					}
					l++;
				}
			}
			if(PLEditor.isVarPrefix(varName)) {
				if(locationList.size()>0){
					locationList.add(new OccurrenceLocation(var.getOffset(), var.getLength(), 0,"desc"));
				} else {
					locationList.add(new OccurrenceLocation(var.getOffset(), var.getLength(), 1,"desc"));				
				}
			}
		} catch (BadLocationException e) {
		}
		
		locations = locationList.toArray(new OccurrenceLocation[0]);
//		if (locations == null) {
////			if (!fStickyOccurrenceAnnotations)
////				removeOccurrenceAnnotations();
////		else
//			if (hasChanged) // check consistency of current annotations
//				removeOccurrenceAnnotations();
//			return;
//		}

		removeOccurrenceAnnotations();
	
		
		fOccurrencesFinderJob= new OccurrencesFinderJob(document, locations);
		//fOccurrencesFinderJob.setPriority(Job.DECORATE);
		//fOccurrencesFinderJob.setSystem(true);
		//fOccurrencesFinderJob.schedule();
		fOccurrencesFinderJob.run(new NullProgressMonitor());
	}
	
	protected boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT);
	}
	
	/**
	 * Element representing a occurrence
	 */
	public static class OccurrenceLocation {
		private final int fOffset;
		private final int fLength;
		private final int fFlags;
		private final String fDescription;

		public OccurrenceLocation(int offset, int length, int flags, String description) {
			fOffset= offset;
			fLength= length;
			fFlags= flags;
			fDescription= description;
		}

		public int getOffset() {
			return fOffset;
		}

		public int getLength() {
			return fLength;
		}

		public int getFlags() {
			return fFlags;
		}

		public String getDescription() {
			return fDescription;
		}

		public String toString() {
			return "[" + fOffset + " / " + fLength + "] " + fDescription; //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		}

	}
	
	private class VarPos {
		int begin;
		int length;
		String prefix;
				
		VarPos(IDocument document, int begin, String prefix) {
			this.begin=begin;
			this.prefix=prefix;
			this.length=prefix.length();
		}
	}
	private TextSelection getVariableAtOffset(IDocument document, int offset)
	throws BadLocationException {
		int begin=offset;
		if(!PLEditor.isNonQualifiedPredicatenameChar(document.getChar(begin)) && begin>0){
			begin--;
		}
		while (PLEditor.isNonQualifiedPredicatenameChar(document
				.getChar(begin))
				&& begin > 0)
			begin--;
		if(begin<offset)
			begin++;
		int end = offset;
		while (PLEditor.isNonQualifiedPredicatenameChar(document
				.getChar(end))
				&& begin > 0)
			end++;
		int length = end - begin;
		String pos = document.get(begin, length);
		
		return new TextSelection(document,begin,length);
	}
	
}