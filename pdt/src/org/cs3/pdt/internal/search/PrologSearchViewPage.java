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
 * Created on 23.08.2004
 *
 */
package org.cs3.pdt.internal.search;

import org.cs3.pdt.internal.ImageRepository;
import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ILabelProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.StructuredViewer;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.search.ui.NewSearchUI;
import org.eclipse.search.ui.text.AbstractTextSearchViewPage;
import org.eclipse.search.ui.text.Match;
import org.eclipse.swt.graphics.Image;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.ITextEditor;


public class PrologSearchViewPage extends AbstractTextSearchViewPage {

	private TextSearchTableContentProvider fContentProvider;
	protected static final Image IMAGE = ImageRepository.getImage(ImageRepository.PE_PUBLIC);

	public PrologSearchViewPage(){
		super(AbstractTextSearchViewPage.FLAG_LAYOUT_FLAT);
		init(NewSearchUI.getSearchResultView().getActivePage().getSite());
	}
	
	protected void elementsChanged(Object[] objects) {
		if (fContentProvider != null)
			fContentProvider.elementsChanged(objects);
		System.out.println("changed");
		
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	protected void clear() {
		if (fContentProvider != null)
			fContentProvider.clear();
		StructuredViewer viewer = getViewer();
		if(viewer!=null){
		    viewer.refresh();
		}
	}

	protected void configureTreeViewer(TreeViewer viewer) {
		// TODO Auto-generated method stub
		System.out.println("confTreeV");
		throw new IllegalStateException("Doesn't support tree mode."); //$NON-NLS-1$
	}

	protected void configureTableViewer(TableViewer viewer) {
		//viewer.setSorter(new JavaElementLineSorter());
		viewer.setLabelProvider(new ILabelProvider(){

			public Image getImage(Object element) {
				//TODO: correct image
				return IMAGE;
			}

			public String getText(Object element) {
				int count = PrologSearchViewPage.this.getDisplayedMatchCount(element);
				String plural = (count==1)?"":"es";
				return ((IFile)element).getFullPath().toString()+ " (" + count +" match"+plural+")";
			}

			public void addListener(ILabelProviderListener listener) {
			}

			public void dispose() {
			}

			public boolean isLabelProperty(Object element, String property) {
				return false;
			}

			public void removeListener(ILabelProviderListener listener) {
			}
			
		});
		fContentProvider= new TextSearchTableContentProvider();
		viewer.setContentProvider(fContentProvider);
		
		
		
	}
	
	protected void showMatch(Match match, int currentOffset, int currentLength, boolean activate) throws PartInitException {
		IEditorPart editor= null;
		IFile file = (IFile)match.getElement();
		try {
			//editor= EditorUtility.openInEditor(file, false);
		    editor = IDE.openEditor(UIUtils.getActivePage(),file);
		} catch (PartInitException e1) {
			return;
		}

		if (editor != null && activate)
			editor.getEditorSite().getPage().activate(editor);
		if (editor instanceof ITextEditor) {
			ITextEditor textEditor= (ITextEditor) editor;
			textEditor.selectAndReveal(currentOffset, currentLength);
		}
	}

	
}