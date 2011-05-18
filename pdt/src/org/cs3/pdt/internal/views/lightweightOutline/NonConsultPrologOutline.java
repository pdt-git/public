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

/*
 * Created on 31.01.2004
 *
 * To change the template for this generated file go to
 * Window - Preferences - Java - Code Generation - Code and Comments
 */
package org.cs3.pdt.internal.views.lightweightOutline;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.views.HideDirectivesFilter;
import org.cs3.pdt.internal.views.HidePrivatePredicatesFilter;
import org.cs3.pdt.internal.views.HideSubtermsFilter;
import org.cs3.pdt.internal.views.PrologOutlineComparer;
import org.cs3.pdt.internal.views.PrologOutlineFilter;
import org.cs3.pl.common.Util;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.AbstractTreeViewer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;



public class NonConsultPrologOutline extends ContentOutlinePage {
	public static final String MENU_ID = "org.cs3.pdt.outline.menu";
	private ITreeContentProvider contentProvider;
	private PrologSourceFileModel model;
	private PLEditor editor;
	private ILabelProvider labelProvider;
	private PrologOutlineFilter[] filters;
	private Menu contextMenu;
	private StringMatcher matcher;
	
	public NonConsultPrologOutline(PLEditor editor) {
		this.editor = editor;
	}

	@Override
	public void createControl(Composite parent) {
		super.createControl(parent);

		TreeViewer viewer = getTreeViewer();
		
		contentProvider = new OutlineContentProvider();
		viewer.setContentProvider(contentProvider);
		
		labelProvider = new OutlineLabelProvider();
		viewer.setLabelProvider(labelProvider);

		viewer.setComparer(new PrologOutlineComparer());

		viewer.addSelectionChangedListener(this);
		
		viewer.setAutoExpandLevel(AbstractTreeViewer.ALL_LEVELS);
		
		model = new PrologSourceFileModel(new ArrayList<OutlinePredicate>());
		
		viewer.setInput(model);

//		String pattern
//		boolean ignoreCase= pattern.toLowerCase().equals(pattern);
//		setfStringMatcher(new StringMatcher(pattern, ignoreCase, false));
//		viewer.addFilter(new NamePatternFilter());
		
		initFilters(); //TODO: hier weiter + sortingAction

		IActionBars actionBars = getSite().getActionBars();
		IToolBarManager toolBarManager = actionBars.getToolBarManager();
		Action action = new LexicalSortingAction(viewer);
		toolBarManager.add(action);
//		Action action = new ToggleSortAction(getTreeViewer());
//		toolBarManager.add(action);
//		action = new FilterActionMenu(this);
//		toolBarManager.add(action);
		
		hookContextMenu(parent);
//		if(getInput() instanceof FileStoreEditorInput){ // TRHO: deactivated for external files 
//			return;
//		}
//		viewer.setInput(input);
		setInput(editor.getEditorInput());
	}


	private void fillContextMenu(IMenuManager manager) {		
		// Other plug-ins can contribute their actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void hookContextMenu(Composite parent) {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			@Override
			public void menuAboutToShow(IMenuManager manager) {
				NonConsultPrologOutline.this.fillContextMenu(manager);
			}
		});
		TreeViewer viewer = getTreeViewer();
		getSite().registerContextMenu(MENU_ID,menuMgr, viewer);
		contextMenu = menuMgr.createContextMenu(parent);
		viewer.getControl().setMenu(contextMenu);
	}

	
	
	public void setInput(Object information) {
		// TODO: Eva: so umbauen, dass information genommen wird (was aber fast das selbe ist)
		String fileName = editor.getPrologFileName();
//		if (information instanceof IEditorInput) {
//			input = ((IEditorInput) information);
//			fileName = ((IEditorInput) information).getName();
//			String string = ((IEditorInput)information).toString();
//			IDocument doc = editor.getDocumentProvider().getDocument(input);
//			String string2 = doc.toString();
//			fileName =((IEditorInput) information).getToolTipText();
//		}
//		if (information instanceof String) {
//			fileName = (String)information;
//		}
		List<OutlinePredicate> predicates;
		if (fileName != "") {
			try {			
			// TODO: Eva: das aus PrologOutlineInformatinControl befreien soweit möglich
				predicates = PrologOutlineInformationControl.getPredicatesForFile(fileName);
				model.update(predicates);
			} catch(Exception e) {
				
			}
		}
		TreeViewer treeViewer = getTreeViewer();
		if (treeViewer != null) {
//			treeViewer.setInput(model);
			treeViewer.refresh();
		}
	}

	@Override
	public void selectionChanged(final SelectionChangedEvent event) {
		super.selectionChanged(event);

		Object elem = getFirstSelectedElement(event);
		if ((elem != null) && (elem instanceof OutlinePredicate)) { 
			OutlinePredicate predicate = (OutlinePredicate)elem;
			editor.gotoLine(predicate.getLine());
		}
	}

	private Object getFirstSelectedElement(final SelectionChangedEvent event) {
		if(event.getSelection().isEmpty()){
			return null;
		}
		if(!(event.getSelection() instanceof IStructuredSelection)){
			return null;
		}
		IStructuredSelection selection = (IStructuredSelection) event.getSelection();
		Object elem = selection.getFirstElement();

		return elem;
	}

	public PrologOutlineFilter[] getAvailableFilters() {
		if (filters == null) {
			filters = new PrologOutlineFilter[] {
					new HideDirectivesFilter("hide_directives",
							"Hide Directives"),
					new HidePrivatePredicatesFilter("hide_private_predicates",
							"Hide Private Predicates"),
					new HideSubtermsFilter("hide_subterms", "Hide Subterms") };
		}
		return filters;
	}

	protected void initFilters() {
		String value = PDTPlugin.getDefault().getPreferenceValue(
				PDT.PREF_OUTLINE_FILTERS, "");
		HashSet<String> enabledIds = new HashSet<String>();
		Util.split(value, ",", enabledIds);
		PrologOutlineFilter[] filters = getAvailableFilters();
		for (int i = 0; i < filters.length; i++) {
			PrologOutlineFilter filter = filters[i];
			if (enabledIds.contains(filter.getId())) {
				getTreeViewer().addFilter(filter);
			}
		}
	}
	

	@Override
	public void dispose() {
		super.dispose();
		contentProvider.dispose();
		model.dispose();
	}
}