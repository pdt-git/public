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

import org.cs3.pdt.internal.contentassistant.NaivPrologContentAssistProcessor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.ui.actions.IJavaEditorActionDefinitionIds;
import org.eclipse.jface.text.AbstractInformationControlManager;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.DefaultInformationControl;
import org.eclipse.jface.text.Document;
import org.eclipse.jface.text.IAutoIndentStrategy;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.IInformationControl;
import org.eclipse.jface.text.IInformationControlCreator;
import org.eclipse.jface.text.IRegion;
import org.eclipse.jface.text.ITextDoubleClickStrategy;
import org.eclipse.jface.text.ITextViewer;
import org.eclipse.jface.text.TextAttribute;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.text.contentassist.ContentAssistant;
import org.eclipse.jface.text.contentassist.IContentAssistant;
import org.eclipse.jface.text.information.IInformationProvider;
import org.eclipse.jface.text.information.InformationPresenter;
import org.eclipse.jface.text.presentation.IPresentationReconciler;
import org.eclipse.jface.text.presentation.PresentationReconciler;
import org.eclipse.jface.text.rules.DefaultDamagerRepairer;
import org.eclipse.jface.text.rules.Token;
import org.eclipse.jface.text.source.IAnnotationHover;
import org.eclipse.jface.text.source.ISourceViewer;
import org.eclipse.jface.text.source.SourceViewerConfiguration;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;

@SuppressWarnings("deprecation")
public class PLConfiguration extends SourceViewerConfiguration {
	private PLDoubleClickStrategy doubleClickStrategy;
	private PLScanner scanner;
	private ColorManager colorManager;
	private IContentAssistant assistant;

	/*
	 * FIXME should not depend on editor
	 * 
	 * 
	 * this was added for because completion, etc. needs some way to associate
	 * the document its working on with a prolog resource - it needs to get a
	 * prolog helper for that project.
	 * 
	 * currently, we assume that completion is only used within the editor. We
	 * can use its input to resolve the connected resource. -> project -> nature ->
	 * plhelper.
	 */
	private PLEditor editor;


	public PLConfiguration(ColorManager colorManager, PLEditor editor) {
		this.colorManager = colorManager;
		this.editor = editor;
	}

	@Override
	public String[] getConfiguredContentTypes(ISourceViewer sourceViewer) {
		return new String[] { IDocument.DEFAULT_CONTENT_TYPE,
				PLPartitionScanner.PL_COMMENT,
				PLPartitionScanner.PL_MULTI_COMMENT };
	}

	@Override
	public ITextDoubleClickStrategy getDoubleClickStrategy(
			ISourceViewer sourceViewer, String contentType) {
		if (doubleClickStrategy == null)
			doubleClickStrategy = new PLDoubleClickStrategy();
		return doubleClickStrategy;
	}

	protected PLScanner getPLScanner() {
		if (scanner == null) {
			reinitScanner();
		}
		return scanner;
	}


	public void reinitScanner() {
		try {
			scanner = new PLScanner(editor, colorManager);
		} catch (CoreException e) {
			Debug.report(e);
		} catch (PrologInterfaceException e) {
		}
		scanner.setDefaultReturnToken(new Token(new TextAttribute(colorManager
				.getColor(IPLColorConstants.DEFAULT))));
	}

	@Override
	public IPresentationReconciler getPresentationReconciler(
			ISourceViewer sourceViewer) {
		PresentationReconciler reconciler = new PresentationReconciler();

		NonRuleBasedDamagerRepairer ndr = new NonRuleBasedDamagerRepairer(
				new TextAttribute(colorManager
						.getColor(IPLColorConstants.PL_COMMENT)));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_MULTI_COMMENT);
		
		DefaultDamagerRepairer dr = new DefaultDamagerRepairer(getPLScanner());
		reconciler.setDamager(dr, IDocument.DEFAULT_CONTENT_TYPE);
		reconciler.setRepairer(dr, IDocument.DEFAULT_CONTENT_TYPE);

		ndr = new NonRuleBasedDamagerRepairer(new TextAttribute(colorManager
				.getColor(IPLColorConstants.PL_COMMENT)));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_COMMENT);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_COMMENT);

		ndr = new NonRuleBasedDamagerRepairer(new TextAttribute(colorManager
				.getColor(IPLColorConstants.STRING)));
		reconciler.setDamager(ndr, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);
		reconciler.setRepairer(ndr, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);

		return reconciler;
	}

	@Override
	public IAutoIndentStrategy getAutoIndentStrategy(
			ISourceViewer sourceViewer, String contentType) {
		return new PLAutoIndentStrategy();
	}

	@Override
	public IAnnotationHover getAnnotationHover(ISourceViewer sourceViewer) {
		return new AnnotationHover();
	}

	@Override
	public IContentAssistant getContentAssistant(ISourceViewer sourceViewer) {
		if (assistant != null)
			return assistant;
		ContentAssistant assistant = null;
		
			assistant = new ContentAssistant();
			assistant.setContentAssistProcessor(
					new NaivPrologContentAssistProcessor(){
						@Override
						protected IFile getFile() {
							if(editor==null){
								return null;
							}
							IEditorInput input = editor.getEditorInput();
							if(input==null){
								return null;
							}
							if(!(input instanceof IFileEditorInput)){
								return null;
							}
							IFileEditorInput input2 = (IFileEditorInput)input;
							IFile file = input2.getFile();
							return file;
						}},
					IDocument.DEFAULT_CONTENT_TYPE);
		
			
		assistant.enableAutoActivation(true);
		assistant.setAutoActivationDelay(500);
		assistant.install(sourceViewer);
		assistant
				.setInformationControlCreator(new IInformationControlCreator() {
					@Override
					public IInformationControl createInformationControl(
							Shell parent) {

						return new DefaultInformationControl(parent);
					}
				});
		this.assistant = assistant;
		return assistant;
	}

	@Override
	public String[] getDefaultPrefixes(ISourceViewer sourceViewer,
			String contentType) {
		return new String[] { "%", "" };
	}

	public InformationPresenter getOutlinePresenter(ISourceViewer sourceViewer) {
		InformationPresenter presenter;
		presenter= new InformationPresenter(getOutlinePresenterControlCreator(sourceViewer, IJavaEditorActionDefinitionIds.SHOW_OUTLINE));
		presenter.setDocumentPartitioning(getConfiguredDocumentPartitioning(sourceViewer));
		presenter.setAnchor(AbstractInformationControlManager.ANCHOR_GLOBAL);
		IInformationProvider provider= new IInformationProvider() {
			
			@Override
			public IRegion getSubject(ITextViewer textViewer, int offset) {
				Document document = (Document) editor.getDocumentProvider().getDocument(
						editor.getEditorInput());


					TextSelection selection = (TextSelection) editor.getEditorSite()
							.getSelectionProvider().getSelection();
					IRegion info;
					try {
						info = document.getLineInformationOfOffset(selection
								.getOffset());
						return info;
					} catch (BadLocationException e) {
						e.printStackTrace();
					}
					return null;
			}
			
			@Override
			public String getInformation(ITextViewer textViewer, IRegion subject) {
				return UIUtils.getFileNameForEditorInput(editor.getEditorInput());
			}
		};
		presenter.setInformationProvider(provider, IDocument.DEFAULT_CONTENT_TYPE);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_COMMENT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_DEFAULT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_DOUBLE_QUOTED_STRING);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_MULTI_COMMENT);
		presenter.setInformationProvider(provider, PLPartitionScanner.PL_SINGLE_QUOTED_STRING);
		presenter.setSizeConstraints(50, 20, true, false);
		presenter.setEnabled(true);
		return presenter;
	}
	
	/**
	 * Returns the outline presenter control creator. The creator is a factory creating outline
	 * presenter controls for the given source viewer. This implementation always returns a creator
	 * for <code>JavaOutlineInformationControl</code> instances.
	 *
	 * @param sourceViewer the source viewer to be configured by this configuration
	 * @param commandId the ID of the command that opens this control
	 * @return an information control creator
	 * @since 2.1
	 */
	private IInformationControlCreator getOutlinePresenterControlCreator(ISourceViewer sourceViewer, final String commandId) {
		return new IInformationControlCreator() {
			public IInformationControl createInformationControl(Shell parent) {
				int shellStyle= SWT.RESIZE;
				int treeStyle= SWT.V_SCROLL | SWT.H_SCROLL;
				return new PrologOutlineInformationControl(editor.getDocumentProvider().getDocument(editor.getEditorInput()),parent, shellStyle, treeStyle, commandId);
			}
		};
	}
}