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
import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.internal.editors.PrologSourceFileModel;
import org.cs3.pdt.internal.editors.StringMatcher;
import org.cs3.pdt.internal.views.PrologFileContentModel;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jdt.ui.IWorkingCopyProvider;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.BusyIndicator;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.PlatformUI;
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

	private OutlineSorter fOutlineSorter;

	private OutlineLabelProvider fInnerLabelProvider;


	private LexicalSortingAction fLexicalSortingAction;
//	private SortByDefiningTypeAction fSortByDefiningTypeAction;

	/**
	 * Category filter action group.
	 * @since 3.2
	 */
	private String fPattern;
	private IDocument document;

	private class OutlineLabelProvider extends LabelProvider implements IColorProvider{//, IStyledLabelProvider {
		@Override
		public String getText(Object element) {
			PrologPredicate prologPredicate = (PrologPredicate)element;
			return prologPredicate.name  +"/" + prologPredicate.arity;
		}

		@Override
		public Image getImage(Object element) {
			PrologPredicate prologPredicate = (PrologPredicate) element;

			if (prologPredicate.isPublic()) {
				return ImageRepository.getImage(ImageRepository.PE_PUBLIC);
			}
			return ImageRepository.getImage(ImageRepository.PE_HIDDEN);
		}

	@Override
	public Color getForeground(Object element) {
		PrologPredicate prologPredicate = (PrologPredicate) element;
		if(prologPredicate.multifile) {
			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_BLUE);
		}
		return null;
	}

	@Override
	public Color getBackground(Object element) {
		PrologPredicate prologPredicate = (PrologPredicate) element;
		if(prologPredicate.dynamic) {
			return PlatformUI.getWorkbench().getDisplay().getSystemColor(SWT.COLOR_GRAY);
		}
		return null;
	}

	}


	private class OutlineTreeViewer extends TreeViewer {

		private boolean fIsFiltering= false;

		private OutlineTreeViewer(Tree tree) {
			super(tree);

		}

		/**
		 * {@inheritDoc}
		 */
		protected Object[] getFilteredChildren(Object parent) {
			Object[] result = getRawChildren(parent);
			int unfilteredChildren= result.length;
			ViewerFilter[] filters = getFilters();
			if (filters != null) {
				for (int i= 0; i < filters.length; i++)
					result = filters[i].filter(this, parent, result);
			}
			fIsFiltering= unfilteredChildren != result.length;
			return result;
		}

	

		
	}


	private class OutlineContentProvider implements ITreeContentProvider, IWorkingCopyProvider  {


		/**
		 * Creates a new Outline content provider.
		 *
		 * @param showInheritedMembers <code>true</code> iff inherited members are shown
		 */
		private OutlineContentProvider() {


		}


		/**
		 * {@inheritDoc}
		 */
		public Object[] getChildren(Object element) {
			if(element instanceof PrologSourceFileModel){
				return ((PrologSourceFileModel)element).getPredicates().toArray();
			}
			return null;
		}

		/**
		 * {@inheritDoc}
		 */
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {

		}

		/**
		 * {@inheritDoc}
		 */
		public void dispose() {

		}


		@Override
		public boolean providesWorkingCopies() {
			return false;
		}


		@Override
		public Object[] getElements(Object element) {
			if(element instanceof PrologSourceFileModel){
				return ((PrologSourceFileModel)element).getPredicates().toArray();
			}
			return null;
		}


		@Override
		public Object getParent(Object element) {
			return null;
		}


		@Override
		public boolean hasChildren(Object element) {
			return false;
		}
	}




	private class OutlineSorter extends ViewerComparator  {

	}


	private class LexicalSortingAction extends Action {

		private static final String STORE_LEXICAL_SORTING_CHECKED= "LexicalSortingAction.isChecked"; //$NON-NLS-1$

		private TreeViewer fOutlineViewer;

		private LexicalSortingAction(TreeViewer outlineViewer) {
			super("lexicalsorting"/*TextMessages.JavaOutlineInformationControl_LexicalSortingAction_label*/, IAction.AS_CHECK_BOX);
			setToolTipText("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_tooltip);
			setDescription("lexicalsorting");//TextMessages.JavaOutlineInformationControl_LexicalSortingAction_description);


			fOutlineViewer= outlineViewer;

//			boolean checked=getDialogSettings().getBoolean(STORE_LEXICAL_SORTING_CHECKED);
			//TODO
			setChecked(true);
//			PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.LEXICAL_SORTING_BROWSING_ACTION);
		}

		public void run() {
			valueChanged(isChecked(), true);
		}

		private void valueChanged(final boolean on, boolean store) {
			setChecked(on);
			BusyIndicator.showWhile(fOutlineViewer.getControl().getDisplay(), new Runnable() {
				public void run() {
					fOutlineViewer.refresh(false);
				}
			});

			if (store)
				getDialogSettings().put(STORE_LEXICAL_SORTING_CHECKED, on);
		}
	}


	private class SortByDefiningTypeAction extends Action {

		private static final String STORE_SORT_BY_DEFINING_TYPE_CHECKED= "SortByDefiningType.isChecked"; //$NON-NLS-1$

		private TreeViewer fOutlineViewer;

		/**
		 * Creates the action.
		 *
		 * @param outlineViewer the outline viewer
		 */
		private SortByDefiningTypeAction(TreeViewer outlineViewer) {
			//TRHO:TODO
			super("sort");//TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_label);
//			setDescription(TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_description);
//			setToolTipText(TextMessages.JavaOutlineInformationControl_SortByDefiningTypeAction_tooltip);
//
//			JavaPluginImages.setLocalImageDescriptors(this, "definingtype_sort_co.gif"); //$NON-NLS-1$

			fOutlineViewer= outlineViewer;

//			PlatformUI.getWorkbench().getHelpSystem().setHelp(this, IJavaHelpContextIds.SORT_BY_DEFINING_TYPE_ACTION);

			//boolean state= getDialogSettings().getBoolean(STORE_SORT_BY_DEFINING_TYPE_CHECKED);
			setChecked(false);
		}

		/*
		 * @see Action#actionPerformed
		 */
		public void run() {
			BusyIndicator.showWhile(fOutlineViewer.getControl().getDisplay(), new Runnable() {
				public void run() {
//					fInnerLabelProvider.setShowDefiningType(isChecked());
					getDialogSettings().put(STORE_SORT_BY_DEFINING_TYPE_CHECKED, isChecked());

					setMatcherString(fPattern, false);
					fOutlineViewer.refresh(true);

					// reveal selection
					Object selectedElement= getSelectedElement();
					if (selectedElement != null)
						fOutlineViewer.reveal(selectedElement);
				}
			});
		}
	}

	/**
	 * String matcher that can match two patterns.
	 *
	 * @since 3.2
	 */
	private static class OrStringMatcher extends StringMatcher {

		private StringMatcher fMatcher1;
		private StringMatcher fMatcher2;

		private OrStringMatcher(String pattern1, String pattern2, boolean ignoreCase, boolean foo) {
			super("", false, false); //$NON-NLS-1$
			fMatcher1= new StringMatcher(pattern1, ignoreCase, false);
			fMatcher2= new StringMatcher(pattern2, ignoreCase, false);
		}

		public boolean match(String text) {
			return fMatcher2.match(text) || fMatcher1.match(text);
		}

	}


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

		fLexicalSortingAction= new LexicalSortingAction(treeViewer);
//		fSortByDefiningTypeAction= new SortByDefiningTypeAction(treeViewer);
		//fCategoryFilterActionGroup= new CategoryFilterActionGroup(treeViewer, getId(), getInputForCategories());

		fOutlineContentProvider= new OutlineContentProvider();
		treeViewer.setContentProvider(fOutlineContentProvider);
		fOutlineSorter= new OutlineSorter();
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
				List<Map<String, Object>> result = session.queryAll("get_pred('" + fileName+"',"+
						"Name,Arity,Line,Dynamic,Multifile,Public)");

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
