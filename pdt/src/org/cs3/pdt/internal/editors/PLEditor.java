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
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.actions.FindDefinitionsActionDelegate;
import org.cs3.pdt.internal.actions.FindPredicateActionDelegate;
import org.cs3.pdt.internal.actions.FindReferencesActionDelegate;
import org.cs3.pdt.internal.actions.ToggleCommentAction;
import org.cs3.pdt.internal.views.lightweightOutline.NonNaturePrologOutline;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.GoalProvider;
import org.cs3.pl.metadata.PredicateReadingUtilities;
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
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.ITypedRegion;
import org.eclipse.jface.text.Position;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.link.LinkedModeModel;
import org.eclipse.jface.text.source.Annotation;
import org.eclipse.jface.text.source.IAnnotationModel;
import org.eclipse.jface.text.source.IAnnotationModelExtension;
import org.eclipse.jface.viewers.IPostSelectionProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.swt.custom.CaretEvent;
import org.eclipse.swt.custom.CaretListener;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.editors.text.TextEditor;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.texteditor.ITextEditorActionDefinitionIds;
import org.eclipse.ui.texteditor.SourceViewerDecorationSupport;
import org.eclipse.ui.texteditor.TextEditorAction;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;

public class PLEditor extends TextEditor {

	public static String COMMAND_SHOW_TOOLTIP = "org.eclipse.pdt.ui.edit.text.prolog.show.prologdoc";

	public static String COMMAND_SHOW_QUICK_OUTLINE = "org.eclipse.pdt.ui.edit.text.prolog.show.quick.outline";

	public static String COMMAND_SAVE_AND_CONSULT = "org.eclipse.pdt.ui.edit.save";

	public static String COMMAND_CONSULT = "org.eclipse.pdt.ui.edit.consult";

	public static String COMMAND_TOGGLE_COMMENTS = "org.eclipse.pdt.ui.edit.text.prolog.toggle.comments";

	private ColorManager colorManager;

	private NonNaturePrologOutline fOutlinePage;

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
		// TRHO: Experimental:
		addProblemMarkers();
		setFocus();

	}

	private void addProblemMarkers() {
		try {
			// current file in editor is either an external file or the pdt nature is not assigned:
			if(!(getEditorInput() instanceof IFileEditorInput)){
				return;
			}
			PLMarkerUtils.updateFileMarkers(((IFileEditorInput)getEditorInput()).getFile());

			
		} catch (CoreException e) {
			e.printStackTrace();
		}
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
			PDTPlugin.getDefault();
			colorManager = PDTPlugin.getDefault().getColorManager();
			
			configuration = new PLConfiguration(colorManager, this);
			setSourceViewerConfiguration(configuration);
			
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	private static final String SEP_PDT_SEARCH = "pdt_search_actions";
	private static final String SEP_PDT_INFO = "pdt_info_actions";
	private static final String SEP_PDT_EDIT = "pdt_edit_actions";
	private static final String SEP_PDT_LAST = "pdt_dummy";

	private IPath filepath;

	private InformationPresenter fOutlinePresenter;

	private TextEditorAction reloadAction;

	

	private static final String MATCHING_BRACKETS = "matching.brackets";

	private static final String MATCHING_BRACKETS_COLOR = "matching.brackets.color";

	public static long OCCURRENCE_UPDATE_DELAY = 300;

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
		
		// TODO: create own bundle
		ResourceBundle bundle = ResourceBundle.getBundle(PDT.RES_BUNDLE_UI);

		createMenuEntryForTooltip(menuMgr, bundle);
		createMenuEntryForOutlinePresenter(menuMgr, bundle);
		createMenuEntryForContentAssist(menuMgr,bundle);

		createMenuEntryForReconsult(menuMgr, bundle);
		createMenuEntryForSaveAndReconsult(menuMgr, bundle);
		
		createMenuEntryForToggleComments(menuMgr, bundle);
		
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
				"Open Primary Declaration", SEP_PDT_SEARCH,
				IJavaEditorActionDefinitionIds.OPEN_EDITOR);		

		addAction(menuMgr, new FindDefinitionsActionDelegate(this),
				"Find all Declarations and Definitions", SEP_PDT_SEARCH,
				IJavaEditorActionDefinitionIds.SEARCH_DECLARATIONS_IN_WORKSPACE);

		addAction(menuMgr, new FindReferencesActionDelegate(this),
				"Find References", SEP_PDT_SEARCH,
				IJavaEditorActionDefinitionIds.SEARCH_REFERENCES_IN_WORKSPACE);

//		addAction(menuMgr, new SpyPointActionDelegate(this),
//				"Toggle Spy Point", SEP_PDT_INSPECT,
//				"org.eclipse.debug.ui.commands.ToggleBreakpoint");
	}

	private void createMenuEntryForTooltip(MenuManager menuMgr,
			ResourceBundle bundle) {
		Action action;
		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ToolTipAction", this) {
			@Override
			public void run() {
				assistant.showContextInformation();
			}
		};
		addAction(menuMgr, action, "Show Tooltip",
				SEP_PDT_INFO, COMMAND_SHOW_TOOLTIP);
	}

	private void createMenuEntryForOutlinePresenter(MenuManager menuMgr,
			ResourceBundle bundle) {
		Action action;
		
		fOutlinePresenter= configuration.getOutlinePresenter(this.getSourceViewer());
		fOutlinePresenter.install(this.getSourceViewer());
		
		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ToolTipAction", this) {
			@Override
			public void run() {
				TextSelection selection = (TextSelection) getEditorSite()
				.getSelectionProvider().getSelection();
				fOutlinePresenter.setOffset(selection.getOffset());
				
				fOutlinePresenter.showInformation();
			}
		};
		addAction(menuMgr, action, "Show Outline",
				SEP_PDT_INFO, COMMAND_SHOW_QUICK_OUTLINE);
	}

	private void createMenuEntryForContentAssist(MenuManager menuMgr, ResourceBundle bundle) {
		assistant = configuration.getContentAssistant(getSourceViewer());
		
		Action action= new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ContextAssistProposal", this) {
			@Override
			public void run() {
				assistant.showPossibleCompletions();
			}
		};
		addAction(menuMgr, action, "Context Assist Proposal",
				SEP_PDT_INFO,
				ITextEditorActionDefinitionIds.CONTENT_ASSIST_PROPOSALS);
	}

	private void createMenuEntryForToggleComments(MenuManager menuMgr,
			ResourceBundle bundle) {
		ToggleCommentAction tca = new ToggleCommentAction(bundle,
				PLEditor.class.getName() + ".ToggleCommentsAction", this);
		tca.configure(getSourceViewer(), configuration);
		tca.setEnabled(true);
		addAction(menuMgr, tca, "Toggle Comments",
				SEP_PDT_LAST, COMMAND_TOGGLE_COMMENTS);
	}

	private void createMenuEntryForReconsult(MenuManager menuMgr,
			ResourceBundle bundle) {
		reloadAction = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".ConsultAction", this) {
			@Override
			public void run() {
				addProblemMarkers();
//				executeConsult();
			}
		};
		addAction(menuMgr, reloadAction, "(Re)consult",
				SEP_PDT_EDIT, COMMAND_CONSULT);
	}

	private void createMenuEntryForSaveAndReconsult(MenuManager menuMgr,
			ResourceBundle bundle) {
		Action action;
		action = new TextEditorAction(bundle, PLEditor.class.getName()
				+ ".SaveAndConsultAction", this) {
			@Override
			public void run() {
				// must be super, otherwise doSave will 
				// consult the file and update the problem markers, too.
				PLEditor.super.doSave(new NullProgressMonitor());
//				addProblemMarkers();
//				setFocus();
			}
		};
		addAction(menuMgr, action, "Save and (Re)consult",
				SEP_PDT_EDIT, COMMAND_SAVE_AND_CONSULT);
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
		menuMgr.add(new Separator(SEP_PDT_SEARCH));	
		menuMgr.add(new Separator(SEP_PDT_INFO));
		menuMgr.add(new Separator(SEP_PDT_EDIT));
		menuMgr.add(new Separator(SEP_PDT_LAST));
//		menuMgr.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
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
//					fOutlinePage = new PrologOutline(this);
					fOutlinePage = new NonNaturePrologOutline(this);
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

	public ContentOutlinePage getOutlinePage() {
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
	 * @param i
	 */
	public void gotoOffset(int offset, int length) {
		Document document = (Document) getDocumentProvider().getDocument(
				getEditorInput());
		TextSelection newSelection;
//		try {
//			if(isLineOffset) {
//				offset = document.getLineOffset(offset);
//			}
			newSelection = new TextSelection(document,offset, length);
			getEditorSite().getSelectionProvider().setSelection(newSelection);
//		} catch (BadLocationException e) {
//			e.printStackTrace();
//		}
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
			int start = PredicateReadingUtilities.findEndOfWhiteSpace(document, info.getOffset(), info
					.getOffset()
					+ info.getLength());

			line = document.get(start, info.getLength() + info.getOffset()
					- start);
		} catch (BadLocationException e) {
			Debug.report(e);

		}
		return line;
	}

	/**
	 * @return
	 */
	public Goal getSelectedPrologElement() throws BadLocationException {
		Document document = (Document) getDocumentProvider().getDocument(
				getEditorInput());

		TextSelection selection = (TextSelection) getEditorSite()
				.getSelectionProvider().getSelection();
		int length = selection.getLength();
		int offset = selection.getOffset();

		return GoalProvider.getPrologDataFromOffset(getPrologFileName(), document, offset, length);
	}

	public String getPrologFileName() {
		return Util.prologFileName(filepath.toFile());
	}

	public TextSelection getSelection() {
		return (TextSelection) getEditorSite().getSelectionProvider()
				.getSelection();
	}

	/**
	 * @param document
	 * @param l
	 * @return
	 */
	public static boolean predicateDelimiter(IDocument document, int l)
			throws BadLocationException {
		if (isDelimitingDot(document,l)) {
			if (l > 0 && isDelimitingDot(document,l-1))
				return false;
			if (l < document.getLength() - 1 && isDelimitingDot(document,l+1))
				return false;
			return true;
		}
		return false;
	}

	/**
	 * Checks if the character at position l is a delimiting dot, meaning it is not enclosed by
	 *  <ul>
	 *    <li>parentheses</li>
	 *    <li>numbers</li>
	 *  </ul>  
	 * @param document
	 * @param l
	 * @return
	 * @throws BadLocationException
	 */
	private static boolean isDelimitingDot(IDocument document, int l) throws BadLocationException {
		char c = document.getChar(l);
		if(c != '.') {
			return false;
		}
		if(l == 0 || l == document.getLength() - 1){
			return false;
		}
		if(isNumber(document.getChar(l-1)) && 
				isNumber(document.getChar(l+1))){
			return false;
		}
		if(document.getChar(l-1)=='(' && 
		   document.getChar(l+1)==')'){
			return false;
		}
		return true;
	}

	private static boolean isNumber(char c) {
		return c >= '0' && c <= '9';
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
	
		filepath = new Path(UIUtils.getFileNameForEditorInput(input));
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
		if(!project.exists()){ // opened external file
			return;
		}

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

	private boolean hightlightOccurrences = true;
	private boolean hightlightSingletonsErrors = true;

	class OccurrencesFinderJob extends Job {

		private final IDocument fDocument;
//		private final ISelectionValidator fPostSelectionValidator;
		private boolean fCanceled= false;
		private Object cancelMonitor = new Object();
		private int fCaretOffset;

		public OccurrencesFinderJob(IDocument document, int caretOffset) {
			super("update occurrences");
			fDocument= document;
			fCaretOffset=caretOffset;

//			if (getSelectionProvider() instanceof ISelectionValidator)
//				fPostSelectionValidator= (ISelectionValidator)getSelectionProvider();
//			else
//				fPostSelectionValidator= null;
		}

		// cannot use cancel() because it is declared final
		void doCancel() {
			fCanceled= true;
			cancel();
			synchronized(cancelMonitor) {
				cancelMonitor.notify();
			}
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
//			System.out.println(Thread.currentThread().getName()+ " wait");
			try {
				synchronized(cancelMonitor) {
					cancelMonitor.wait(OCCURRENCE_UPDATE_DELAY);
				}
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			if (isCanceled(progressMonitor)) {
//				System.out.println(Thread.currentThread().getName()+ " cancelled");
				return Status.CANCEL_STATUS;
			}
//			System.out.println(Thread.currentThread().getName()+ " not cancelled");

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
			ArrayList<OccurrenceLocation> locationList = parseOccurrences(fCaretOffset, document);
			if(locationList == null){
//				removeOccurrenceAnnotations();
				return Status.CANCEL_STATUS;
			}
			
			int length= locationList.size();
			Map<Annotation, Position> annotationMap= new HashMap<Annotation, Position>(length);
			for (int i= 0; i < length; i++) {

				if (isCanceled(progressMonitor))
					return Status.CANCEL_STATUS;

				OccurrenceLocation location= locationList.get(i);
				Position position= new Position(location.getOffset(), location.getLength());

				String description= location.getDescription();
				String annotationType;
				switch(location.getFlags()){
				case 0:
					annotationType="org.cs3.pdt.occurrences";
					break;
				case 1:
					annotationType="org.cs3.pdt.occurrences.singleton";
					break;
				case 2:
					annotationType="org.cs3.pdt.occurrences.non.singleton";
					break;
				case 3:
					annotationType="org.cs3.pdt.occurrences.singleton.wrong.prefix";
					break;
					default:
						annotationType="org.cs3.pdt.occurrences";
				}
				
				annotationMap.put(new Annotation(annotationType, false, description), position);
			}

			if (isCanceled(progressMonitor))
				return Status.CANCEL_STATUS;
			synchronized (annotationModelMonitor) {

				//removeOccurrenceAnnotations();

				if (annotationModel instanceof IAnnotationModelExtension) {
					//((IAnnotationModelExtension)annotationModel).removeAllAnnotations();
					((IAnnotationModelExtension)annotationModel).replaceAnnotations(fOccurrenceAnnotations, annotationMap);
				} else {
					removeOccurrenceAnnotations();
					Iterator<Map.Entry<Annotation, Position>> iter= annotationMap.entrySet().iterator();
					while (iter.hasNext()) {
						Map.Entry<Annotation, Position> mapEntry= iter.next();
						annotationModel.addAnnotation((Annotation)mapEntry.getKey(), (Position)mapEntry.getValue());
					}
				}
				fOccurrenceAnnotations= annotationMap.keySet().toArray(new Annotation[annotationMap.keySet().size()]);
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

		if(!hightlightOccurrences ){
			return;
		}
		
		if (fOccurrencesFinderJob != null)
			fOccurrencesFinderJob.doCancel();
		

		if (!fMarkOccurrenceAnnotations )
			return;

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

		
//		if (locations == null) {
////			if (!fStickyOccurrenceAnnotations)
////				removeOccurrenceAnnotations();
////		else
//			if (hasChanged) // check consistency of current annotations
//				removeOccurrenceAnnotations();
//			return;
//		}

	
		synchronized (annotationModelMonitor) {
			fOccurrencesFinderJob= new OccurrencesFinderJob(document,caretOffset);
			fOccurrencesFinderJob.setPriority(Job.DECORATE);
			fOccurrencesFinderJob.setSystem(true);
			fOccurrencesFinderJob.schedule();
		}
		//fOccurrencesFinderJob.run(new NullProgressMonitor());
	}
	/**
	 * Looks up all occurrences of (singleton) variables at the current location.
	 * 
	 * @param caretOffset
	 * @param document
	 * @return
	 */
	private ArrayList<OccurrenceLocation> parseOccurrences(int caretOffset, IDocument document) {
		ArrayList<OccurrenceLocation> locationList = new ArrayList<OccurrenceLocation>();
		Map<String, List<OccurrenceLocation>> singletonOccurs = new HashMap<String, List<OccurrenceLocation>>(); 
		Map<String, List<OccurrenceLocation>> nonSingletonOccurs = new HashMap<String, List<OccurrenceLocation>>(); 
		
		try {
			TextSelection var= getVariableAtOffset(document,caretOffset);
			String varName = var.getText();
			if(oldSelection!=null && oldSelection.equals(var)){
				return null;
			} else {
				oldSelection = var;
			}
			int begin=var.getOffset();
				int l = begin == 0 ? begin : begin - 1;
				String proposal = null;
				while (l > 0) {
					ITypedRegion region = document.getPartition(l);
					if (isComment(region))
						l = region.getOffset();
					else {
						if(PLEditor.predicateDelimiter(document, l)){
							proposal = processProposal(singletonOccurs,nonSingletonOccurs, locationList, var, l,true,proposal);
							break;
						} 
						proposal = processProposal(singletonOccurs,nonSingletonOccurs, locationList, var, l,true,proposal);
					}
					l--;
				}
				
				// searching downwards
				l = begin = var.getOffset()+ var.getLength();
				proposal = null;
				while (l < document.getLength()) {
					ITypedRegion region = document.getPartition(l);
					if (isComment(region)) {
						l = region.getOffset() + region.getLength();
					} else {
						if(PLEditor.predicateDelimiter(document, l)){
							proposal = processProposal(singletonOccurs,nonSingletonOccurs, locationList, var, l,false,proposal);
							break;
						}
						proposal = processProposal(singletonOccurs,nonSingletonOccurs, locationList, var, l,false,proposal);
					}
					l++;
				}
			if(hightlightSingletonsErrors){
				for (String varToCheck : singletonOccurs.keySet()) {
					List<OccurrenceLocation> varLocations = singletonOccurs.get(varToCheck);
					if(varLocations.size()>1){
						for (OccurrenceLocation varLocation : varLocations) {
							locationList.add(varLocation);
						}
					}
				}
				for (String varToCheck : nonSingletonOccurs.keySet()) {
					List<OccurrenceLocation> varLocations = nonSingletonOccurs.get(varToCheck);
					if(varLocations.size()==1){
						for (OccurrenceLocation varLocation : varLocations) {
							locationList.add(varLocation);
						}
					}
				}

			}
			
			if(Util.isVarPrefix(varName)) {
				if(locationList.size()>0){
					locationList.add(new OccurrenceLocation(var.getOffset(), var.getLength(), 0,"desc"));
				} else {
					locationList.add(new OccurrenceLocation(var.getOffset(), var.getLength(), 1,"desc"));				
				}
			}
		} catch (BadLocationException e) {
		}
		return locationList;
	}

	private String processProposal(
			Map<String, List<OccurrenceLocation>> singletonOccurs,
			Map<String, List<OccurrenceLocation>> nonSingletonOccurs,
			ArrayList<OccurrenceLocation> locationList, TextSelection var,
			int l, boolean up, String proposal) throws BadLocationException {
		char c = getSourceViewer().getDocument().getChar(l);

		if (Util.isVarChar(c)) {
			if (proposal == null)
				proposal = "";
			if(up){
				proposal = c + proposal;
			}else{
				proposal = proposal+c;				
			}
		} else if (proposal != null) {
			int length = proposal.length();
				if(var.getText().equals(proposal)) {
					locationList.add(new OccurrenceLocation(l+(up?1:-length), length, 0,"desc"));
				} else if(Util.isVarPrefix(proposal) && !proposal.equals("_") ){
					List<OccurrenceLocation> probOccs;
					int kind = 2;
					if(isSingletonName(proposal)==VAR_KIND_SINGLETON){
						probOccs=singletonOccurs.get(proposal);
						if(probOccs==null){
							probOccs =new ArrayList<OccurrenceLocation>();
							singletonOccurs.put(proposal, probOccs);
						}
						kind = 3;
					} else {
						probOccs=nonSingletonOccurs.get(proposal);
						if(probOccs==null){
							probOccs =new ArrayList<OccurrenceLocation>();
							nonSingletonOccurs.put(proposal, probOccs);
						}
					}
					probOccs.add(new OccurrenceLocation(l+(up?1:-length), length, kind,"desc"));
				}
			proposal = null;
		}
		return proposal;
	}
	
	
	public static final int VAR_KIND_ANONYMOUS = 0;
	public static final int VAR_KIND_SINGLETON = 1;
	public static final int VAR_KIND_NORMAL=2;
	
	/**
	 * @param a valid Prolog variable
	 */
	private static int isSingletonName(String proposal) {
		if(proposal.equals("_")){
			return VAR_KIND_ANONYMOUS;
		}
		if(proposal.length()==1){
			return VAR_KIND_NORMAL;
		}
		if(proposal.charAt(0)=='_' && Util.isSingleSecondChar(proposal.charAt(1))){
			return VAR_KIND_SINGLETON;
		}
		return VAR_KIND_NORMAL;
	}

	
	protected boolean isComment(ITypedRegion region) {
		return region.getType().equals(PLPartitionScanner.PL_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_MULTI_COMMENT)
				|| region.getType().equals(PLPartitionScanner.PL_SINGLE_QUOTED_STRING)
				|| region.getType().equals(PLPartitionScanner.PL_DOUBLE_QUOTED_STRING);
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
	
//	private class VarPos {
//		int begin;
//		int length;
//		String prefix;
//				
//		VarPos(IDocument document, int begin, String prefix) {
//			this.begin=begin;
//			this.prefix=prefix;
//			this.length=prefix.length();
//		}
//	}
	
	private TextSelection getVariableAtOffset(IDocument document, int offset)
	throws BadLocationException {
		int begin=offset;
		if(!Util.isNonQualifiedPredicateNameChar(document.getChar(begin)) && begin>0){
			begin--;
		}
		while (Util.isNonQualifiedPredicateNameChar(document
				.getChar(begin))
				&& begin > 0)
			begin--;
		if(begin<offset)
			begin++;
		int end = offset;
		while (Util.isNonQualifiedPredicateNameChar(document
				.getChar(end))
				&& begin > 0)
			end++;
		int length = end - begin;
//		String pos = document.get(begin, length);
		
		return new TextSelection(document,begin,length);
	}
	
}