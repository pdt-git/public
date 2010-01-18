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
package org.cs3.pdt.internal.views;

import java.io.File;
import java.util.HashSet;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.IElementComparer;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;



public class PrologOutline extends ContentOutlinePage {
	public static final String MENU_ID = "org.cs3.pdt.outline.menu";
	private ITreeContentProvider contentProvider;
	private PrologFileContentModel model;
	private boolean convertPositions;
	private PLEditor editor;
	private ILabelProvider labelProvider;
	private PrologOutlineFilter[] filters;
	private IEditorInput input;
	private TreeViewer viewer;
	private Menu contextMenu;
	
	private final class Comparer implements IElementComparer {
		public int hashCode(Object element) {
			if (element instanceof Predicate) {
				Predicate p = (Predicate) element;
				return p.getSignature().hashCode();
			}
			return element.hashCode();
		}

		public boolean equals(Object a, Object b) {
			if (a instanceof Predicate && b instanceof Predicate) {
				Predicate pa = (Predicate) a;
				Predicate pb = (Predicate) b;
				return pa.getSignature().equals(pb.getSignature());
			}
			return a.equals(b);
		}
	}

	public PrologOutline(PLEditor editor) {
		this.editor = editor;
	}

	public void createControl(Composite parent) {
		super.createControl(parent);

		viewer = getTreeViewer();
		model = new ContentModel() {

			public File getFile() {
				try {
					IFileEditorInput editorInput = null;
					IFile file = null;
					IPrologProject plProject = null;

					if (input instanceof IFileEditorInput) {
						editorInput = (IFileEditorInput) input;
					}
					if (editorInput != null) {
						file = editorInput.getFile();
						plProject = PDTCoreUtils.getPrologProject(file);
					}

					if (plProject == null) {
						file = null;
					}
					return file == null ? null : file.getLocation().toFile();
				} catch (Exception e) {
					return null;
				}
			}

		};

		contentProvider = new CTermContentProvider(viewer, model);
		labelProvider = new PEFLabelProvider();
		viewer.setContentProvider(contentProvider);
		viewer.setLabelProvider(labelProvider);

		this.convertPositions = true;

		viewer.setComparer(new Comparer());

		viewer.addSelectionChangedListener(this);

		initFilters();

		IActionBars actionBars = getSite().getActionBars();
		IToolBarManager toolBarManager = actionBars.getToolBarManager();
		Action action = new ToggleSortAction(this);
		toolBarManager.add(action);
		action = new FilterActionMenu(this);
		toolBarManager.add(action);
		
		hookContextMenu(parent);
		
		viewer.setInput(getInput());

	}


	private void fillContextMenu(IMenuManager manager) {		
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}
	
	private void hookContextMenu(Composite parent) {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				PrologOutline.this.fillContextMenu(manager);
			}
		});
		getSite().registerContextMenu(MENU_ID,menuMgr, viewer);
		contextMenu = menuMgr.createContextMenu(parent);
		viewer.getControl().setMenu(contextMenu);
	}
	
	
	public TreeViewer getTreeViewer() {
		return super.getTreeViewer();
	}

	public void setInput(IEditorInput input) {
		this.input = input;
		TreeViewer viewer = getTreeViewer();
		if (viewer != null) {
			viewer.setInput(input);
		}

	}

	public IEditorInput getInput() {
		getTreeViewer();
		return input;
	}

	public void selectionChanged(final SelectionChangedEvent event) {
		super.selectionChanged(event);
		
		if(event.getSelection().isEmpty()){
			return;
		}
		if(!(event.getSelection() instanceof IStructuredSelection)){
			return;
		}
		IStructuredSelection selection = (IStructuredSelection) event.getSelection();
		Object elm = selection.getFirstElement();
		if(elm==null||!(elm instanceof PEFNode)){
			return;
		}
		PEFNode node = (PEFNode)elm;
		int startOffset = node.getStartPosition();
		int endOffset = node.getEndPosition();
		if (convertPositions) {
			IDocument doc = editor.getDocumentProvider()
					.getDocument(getInput());
			if (doc == null) {
				// wunder, gr�bel,...
				Debug.debug("Debug: input=" + getInput());
			}
			try {
				startOffset = PDTCoreUtils.convertLogicalToPhysicalOffset(doc.get(),
						startOffset);
				endOffset = PDTCoreUtils.convertLogicalToPhysicalOffset(doc.get(),
						endOffset);
				Debug.debug(">>"
						+ doc.get(startOffset, endOffset - startOffset) + "<<");
				Debug.debug(">>>" + doc.get().substring(startOffset, endOffset)
						+ "<<<");
			} catch (BadLocationException e) {
				Debug.warning("bad location: "+startOffset+", "+endOffset);
			}
		}

		if (startOffset >= 0 && endOffset >= 0) {
			PLEditor editor = ((PLEditor) UIUtils.getActiveEditor());
			if(editor==null){
				return ;
			}
			editor.selectAndReveal(startOffset, endOffset - startOffset);
		}
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

	public void dispose() {

		super.dispose();
		contentProvider.dispose();
		model.dispose();
	}
}