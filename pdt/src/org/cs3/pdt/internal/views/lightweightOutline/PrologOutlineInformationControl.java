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

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.internal.editors.PrologSourceFileModel;
import org.cs3.pdt.internal.views.PrologFileContentModel;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.keys.KeySequence;
import org.eclipse.ui.keys.SWTKeySupport;
//import org.eclipse.jface.bindings.keys.SWTKeySupport;
//import org.eclipse.jface.bindings.keys.KeySequence;

/**
 * Show outline in light-weight control.
 *
 * @since 2.1
 */
public class PrologOutlineInformationControl extends AbstractInformationControl {

	private KeyAdapter fKeyAdapter;
	private OutlineContentProvider fOutlineContentProvider;
	private PrologSourceFileModel fInput= null;

	private ViewerComparator fOutlineSorter;

	private OutlineLabelProvider fInnerLabelProvider;


	private LexicalSortingAction fLexicalSortingAction;
//	private SortByDefiningTypeAction fSortByDefiningTypeAction;

	/**
	 * Category filter action group.
	 * @since 3.2
	 */
	String fPattern;
	private IDocument document;

	/**
	 * Creates a new Java outline information control.
	 * @param iDocument 
	 *
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
		treeViewer.addFilter(new NamePatternFilter());
		//treeViewer.addFilter(new MemberFilter());

		fInnerLabelProvider= new OutlineLabelProvider();
//		fInnerLabelProvider.addLabelDecorator(new ProblemsLabelDecorator(null));
//		IDecoratorManager decoratorMgr= PlatformUI.getWorkbench().getDecoratorManager();
//		if (decoratorMgr.getEnabled("org.eclipse.jdt.ui.override.decorator")) //$NON-NLS-1$
//			fInnerLabelProvider.addLabelDecorator(new OverrideIndicatorLabelDecorator(null));

		//TODO
		treeViewer.setLabelProvider(fInnerLabelProvider);

		fLexicalSortingAction= new LexicalSortingAction(this, treeViewer);
//		fSortByDefiningTypeAction= new SortByDefiningTypeAction(treeViewer);
		//fCategoryFilterActionGroup= new CategoryFilterActionGroup(treeViewer, getId(), getInputForCategories());

		fOutlineContentProvider= new OutlineContentProvider();
		treeViewer.setContentProvider(fOutlineContentProvider);
		fOutlineSorter= new ViewerComparator();
		treeViewer.setComparator(fOutlineSorter);
		treeViewer.setAutoExpandLevel(AbstractTreeViewer.ALL_LEVELS);


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

	private InputStream mockStream(String inputText){
		try {
			return new ByteArrayInputStream(inputText.getBytes());
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	/**
	 * {@inheritDoc}
	 */
	public void setInput(Object information) {
		if(information instanceof String) {
			String fileName = (String)information;
			List<PrologPredicate> predicates= new ArrayList<PrologPredicate>();
			PrologSession session=null;
			try {
				PrologConsole console = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
				if(console==null){
					MessageBox messageBox = new MessageBox(
							getShell(), SWT.ICON_WARNING| SWT.OK);

					messageBox.setText("Outline");
					messageBox.setMessage("Cannot open outline, no factbase selected in Prolog Console.");
					messageBox.open();
					return;
				}
				session = console.getPrologInterface().getSession();
				String query = "get_pred('" + fileName+"',"+"Name,Arity,Line,Dynamic,Multifile,Public)";
				List<Map<String, Object>> result = session.queryAll(query);

				Set<String> names = new HashSet<String>();
				for (Map<String, Object> predicate : result) {
					String name=(String)predicate.get("Name");
					int arity=Integer.parseInt((String)predicate.get("Arity"));
					String signature = name+arity;
					if(!names.contains(signature)){
						names.add(signature);
						PrologPredicate prologPredicate = new PrologPredicate();
						prologPredicate.name=name;
						prologPredicate.arity=arity;
						prologPredicate.setPublic(predicate.get("Public").equals("1"));
						prologPredicate.setMultifile(predicate.get("Multifile").equals("1"));
						prologPredicate.setDynamic(predicate.get("Dynamic").equals("1"));
						
						prologPredicate.line=Integer.parseInt((String)predicate.get("Line"));
						predicates.add(prologPredicate);
					}
				}
			}catch(Exception e){
				Debug.report(e);
			} finally {
				if(session!=null)session.dispose();
			}
			
			 
//			
		PrologSourceFileModel model = new PrologSourceFileModel(predicates,fileName);
		inputChanged(model, predicates.size()>0?predicates.get(0):null);
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

//		viewMenu.add(fSortByDefiningTypeAction);

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
		fStringMatcher= new OrStringMatcher(pattern, pattern2, ignoreCase, false);

		if (update)
			stringMatcherUpdated();

	}

	public void gotoSelectedElement() {
		PrologPredicate predicate= (PrologPredicate)getSelectedElement();					
		ISelection selection;
		try {
			selection = new TextSelection(document,document.getLineOffset(predicate.line-1),0);
			UIUtils.getActiveEditor().getEditorSite().getSelectionProvider().setSelection(selection);
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


//	CanonicalTermParser termParser = new CanonicalTermParser(mockStream(document.get()));
//	try {
//		termParser.Term();
//		Node root = termParser.getASTRoot();
//	int num = root.jjtGetNumChildren();
//	Set<String> names = new HashSet<String>();
//	for(int i = 0;i<num;i++){
//		Node child=root.jjtGetChild(i);
//		if(child instanceof ASTCompound){
//			ASTCompound comp = (ASTCompound)child;
//			String signature = comp.getFunctor() + "/" + comp.jjtGetNumChildren();
//			if(!names.contains(signature)){
//				names.add(signature);
//				PrologPredicate prologPredicate = new PrologPredicate();
//				prologPredicate.name=comp.getFunctor();
//				prologPredicate.arity=comp.jjtGetNumChildren();
//				prologPredicate.line=comp.jjtGetFirstToken().beginLine;
//				predicates.add(prologPredicate);
//				
//			}
//			
//		}
//	}
//	} catch(Throwable e){
//		e.printStackTrace();
//	}
}
