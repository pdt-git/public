package org.cs3.pdt.internal.views.lightweightOutline;

/*******************************************************************************
 * Copyright (c) 2000, 2009 IBM Corporation and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     IBM Corporation - initial API and implementation
 *******************************************************************************/

import java.util.Map;

import org.cs3.pdt.internal.queries.PDTOutlineQuery;
import org.cs3.pdt.internal.structureElements.OutlineModuleElement;
import org.cs3.pdt.internal.structureElements.OutlinePredicate;
import org.cs3.pdt.internal.structureElements.PredicateOccuranceElement;
import org.cs3.pdt.internal.views.PrologFileContentModel;
import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.DecoratingLabelProvider;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.keys.KeySequence;
import org.eclipse.ui.keys.SWTKeySupport;

/**
 * Show outline in light-weight control.
 *
 * @since 2.1
 */
public class PrologOutlineInformationControl extends AbstractInformationControl {

	private OutlineContentProvider fOutlineContentProvider;
	private PrologSourceFileModel fInput;
	private ViewerComparator fOutlineSorter;
	private LabelProvider fInnerLabelProvider;
	private LexicalSortingAction fLexicalSortingAction;
	/**
	 * Category filter action group.
	 * @since 3.2
	 */
	public String fPattern;
	private IDocument document;
	private KeyAdapter fKeyAdapter;

	/**
	 * Creates a new Java outline information control.
	 * @param iDocument 
	 * @param parent
	 * @param shellStyle
	 * @param treeStyle
	 * @param commandId
	 */
	public PrologOutlineInformationControl(IDocument document, Shell parent, int shellStyle, int treeStyle, String commandId) {
		super(parent, shellStyle, treeStyle, commandId, true);
		this.document = document;
	}

	/**
	 * {@inheritDoc}
	 */
	protected Text createFilterText(Composite parent) {
		Text text= super.createFilterText(parent);
		text.addKeyListener(getKeyAdapter());
		return text;
	}

	/**
	 * {@inheritDoc}
	 */
	protected TreeViewer createTreeViewer(Composite parent, int style) {
		Tree tree= new Tree(parent, SWT.SINGLE | (style & ~SWT.MULTI));
		GridData gd= new GridData(GridData.FILL_BOTH);
		gd.heightHint= tree.getItemHeight() * 12;
		tree.setLayoutData(gd);

		final TreeViewer treeViewer= new OutlineTreeViewer(tree);
		treeViewer.addDoubleClickListener(new IDoubleClickListener() {
			
			@Override
			public void doubleClick(DoubleClickEvent event) {
				gotoSelectedElement();
			}
		});
		// Hard-coded filters
		treeViewer.addFilter(new NamePatternFilter(this, this.getMatcher()));


		//fInnerLabelProvider= new OutlineLabelProvider();
		fInnerLabelProvider =  new DecoratingLabelProvider(new OutlineLabelProvider(), 
				PlatformUI.getWorkbench().getDecoratorManager().getLabelDecorator());
		treeViewer.setLabelProvider(fInnerLabelProvider);

		fLexicalSortingAction= new LexicalSortingAction(/*this,*/ treeViewer);

		fOutlineContentProvider= new OutlineContentProvider();
		treeViewer.setContentProvider(fOutlineContentProvider);
		fOutlineSorter= new ViewerComparator();
		treeViewer.setComparator(fOutlineSorter);
		//treeViewer.setAutoExpandLevel(AbstractTreeViewer.ALL_LEVELS);
		treeViewer.setAutoExpandLevel(2);

		treeViewer.getTree().addKeyListener(getKeyAdapter());

		return treeViewer;
	}

	/**
	 * {@inheritDoc}
	 */
	protected String getStatusFieldText() {
		return "";
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#getId()
	 * @since 3.0
	 */
	protected String getId() {
		return "org.eclipse.jdt.internal.ui.text.QuickOutline"; //$NON-NLS-1$
	}

	/**
	 * {@inheritDoc}
	 */
	public void setInput(Object information) {
		if(information instanceof String) {
			String fileName = (String)information;
			Map<String, OutlineModuleElement> modules = PDTOutlineQuery.getProgramElementsForFile(fileName/*, getShell()*/);

			PrologSourceFileModel model = new PrologSourceFileModel(modules);
			inputChanged(model, modules.size()>0?modules.get(0):null);
			fInput=model;
			return;
		}
		if (information == null || information instanceof String) {
			inputChanged(null, null);
			return;
		}

		inputChanged(fInput, (PrologFileContentModel)information);

	}

	private KeyAdapter getKeyAdapter() {
		if (fKeyAdapter == null) {
			fKeyAdapter= new KeyAdapter() {
				public void keyPressed(KeyEvent e) {
					int accelerator = SWTKeySupport.convertEventToUnmodifiedAccelerator(e);
					KeySequence keySequence = KeySequence.getInstance(SWTKeySupport.convertAcceleratorToKeyStroke(accelerator));
					KeySequence[] sequences= getInvokingCommandKeySequences();
					if (sequences == null){
						if(accelerator==13){
							gotoSelectedElement();
						}
						return;
					}
					for (int i= 0; i < sequences.length; i++) {
						if (sequences[i].equals(keySequence)) {
							e.doit= false;
							return;
						}
					}
				}
			};
		}
		return fKeyAdapter;
	}



	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#fillViewMenu(org.eclipse.jface.action.IMenuManager)
	 */
	protected void fillViewMenu(IMenuManager viewMenu) {
		super.fillViewMenu(viewMenu);

		viewMenu.add(new Separator("Sorters")); //$NON-NLS-1$
		viewMenu.add(fLexicalSortingAction);
	}

	/*
	 * @see org.eclipse.jdt.internal.ui.text.AbstractInformationControl#setMatcherString(java.lang.String, boolean)
	 * @since 3.2
	 */
	protected void setMatcherString(String pattern, boolean update) {
		fPattern= pattern;
		if (pattern.length() == 0) {
			super.setMatcherString(pattern, update);
			return;
		}

		boolean ignoreCase= pattern.toLowerCase().equals(pattern);
		String pattern2= "*" + pattern; //$NON-NLS-1$
		setfStringMatcher(new OrStringMatcher(pattern, pattern2, ignoreCase, false));

		if (update)
			stringMatcherUpdated();

	}

	public void gotoSelectedElement() {
		Object selection = getSelectedElement();
		int line = -1;
		if (selection instanceof OutlinePredicate) {
			OutlinePredicate predicate=(OutlinePredicate)selection;
			line = predicate.getLine()-1;
		} 
		if (selection instanceof PredicateOccuranceElement) {
			PredicateOccuranceElement occurance = (PredicateOccuranceElement)selection;
			line = occurance.getLine()-1;
		}
		ISelection textSelection;
		try {
			textSelection = new TextSelection(document,document.getLineOffset(line),0);
			UIUtils.getActiveEditor().getEditorSite().getSelectionProvider().setSelection(textSelection);
		} catch (BadLocationException e1) {
			e1.printStackTrace();
		}
		close();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.PopupDialog#getDialogSettings()
	 */
	@Override
	// override for visibility reasons
	//TODO: super call always returns null - maybe it's a good idea to redo this somehow
	protected IDialogSettings getDialogSettings() {
		return super.getDialogSettings();
	}
}
