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

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.cterm.CCompound;
import org.cs3.pl.cterm.CInteger;
import org.cs3.pl.cterm.CTerm;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Directive;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.metadata.SourceLocation;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IElementComparer;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.texteditor.AbstractTextEditor;
import org.eclipse.ui.texteditor.IDocumentProvider;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class PrologOutline extends ContentOutlinePage {
	private final class MyViewSorter extends ViewerSorter {
		public int category(Object element) {
			if(element instanceof Directive){
				return 0;
			}
			if(element instanceof Clause){
				return 1;
			}
			if(element instanceof Predicate){
				return 2;
			}
			if(element instanceof CTerm){
				return 3;
			}
			return 4;
		}

		public int compare(Viewer viewer, Object e1, Object e2) {
			if(e1 instanceof Directive && e2 instanceof Directive){
				return ((Comparable)e1).compareTo(e2);
			}
			if(e1 instanceof Clause && e2 instanceof Clause){
				return ((Comparable)e1).compareTo(e2);
			}
			if(e2 instanceof CTerm && e2 instanceof CTerm){
				CTerm t1 = (CTerm) e1;
				CTerm t2 = (CTerm) e2;
				CCompound pos1 = (CCompound) t1.getAnotation("position");
				CCompound pos2 = (CCompound) t2.getAnotation("position");
				int TOP = Integer.MAX_VALUE;
				int start1=TOP;
				int start2=TOP;
				int end1=TOP;
				int end2=TOP;
				if (pos1 != null) { // can be null, e.g. for implicit NILs
					start1 = ((CInteger) pos1.getArgument(0)).getIntValue();
					end1 = ((CInteger) pos1.getArgument(1)).getIntValue();
				}
				if (pos2 != null) { // can be null, e.g. for implicit NILs
					start2 = ((CInteger) pos2.getArgument(0)).getIntValue();
					end2 = ((CInteger) pos2.getArgument(1)).getIntValue();
				}
				int c = start1-start2;
				if(c!=0){
					return c;
				}
				c=end1-end2;
				if(c!=0){
					return c;
				}
				
			}
			if( e1 instanceof Predicate && e2 instanceof Predicate){
				Predicate p1=(Predicate)e1;
				Predicate p2=(Predicate)e2;
				return p1.compareTo(p2);
			}
			return super.compare(viewer, e1, e2);
		}
	}

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

	IEditorInput input;

	
	// private TreeViewer viewer;
	private ITreeContentProvider contentProvider;


	private boolean convertPositions;


	private PLEditor editor;


	private PrologElementLabelProvider labelProvider;


	public PrologOutline(PLEditor editor) {
		this.editor=editor;
	}

	public void createControl(Composite parent) {
		super.createControl(parent);

		TreeViewer viewer = getTreeViewer();

		
			contentProvider=new CTermContentProvider(viewer);
			labelProvider=new PrologElementLabelProvider();
			viewer.setContentProvider(contentProvider);
			viewer.setLabelProvider(labelProvider);
			viewer.setSorter(new MyViewSorter());	
			this.convertPositions=true;
		
		viewer.setComparer(new Comparer());
		
		viewer.addSelectionChangedListener(this);
		setInput(input);
		

		

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
		return input;
	}

	/**
	 * @param input2
	 * @param b
	 * @return
	 */

	public void selectionChanged(SelectionChangedEvent event) {
		super.selectionChanged(event);
		if (!((StructuredSelection) event.getSelection()).isEmpty()) {
			Object elm = ((StructuredSelection) event.getSelection())
					.getFirstElement();

			int startOffset = -1;
			int endOffset = -1;

			SourceLocation loc;
			if (elm instanceof CTermNode) {
				CTerm term = ((CTermNode) elm).term;
				CCompound posterm = (CCompound) term.getAnotation("position");
				if (posterm != null) { // can be null, e.g. for implicit NILs
					startOffset = ((CInteger) posterm.getArgument(0)).getIntValue();
					endOffset = ((CInteger) posterm.getArgument(1)).getIntValue();
				}
			} else if (elm instanceof Clause) {
				Clause c = (Clause) elm;
				loc = c.getSourceLocation();
				startOffset = loc.offset;
				endOffset = loc.endOffset;
			}
			if (elm instanceof Directive) {
				Directive c = (Directive) elm;
				loc = c.getSourceLocation();
				startOffset = loc.offset;
				endOffset = loc.endOffset;
			} else if (elm instanceof Predicate) {
				Object[] children = getContentProvider().getChildren(elm);
				if (children == null || children.length == 0) {
					return;
				}
				if(!(children[0] instanceof Clause)){
					//XXX: children may not have been fetched yet, in which case there is only a pseudo child.
					// we need a way to wait for children.
					return;
				}
				Clause c = (Clause) children[0];
				loc = c.getSourceLocation();
				startOffset = loc.offset;
				endOffset = loc.endOffset;
			}

			if(convertPositions){
				IDocument doc=editor.getDocumentProvider().getDocument(getInput());
				
				try {
					startOffset=PDTUtils.logicalToPhysicalOffset(doc.get(),startOffset);
					endOffset=PDTUtils.logicalToPhysicalOffset(doc.get(),endOffset);
					Debug.debug(">>"+doc.get(startOffset,endOffset-startOffset)+"<<");
					Debug.debug(">>>"+doc.get().substring(startOffset,endOffset)+"<<<");
				} catch (BadLocationException e) {
					Debug.rethrow(e);
				}
			}
			
			if (startOffset >= 0 && endOffset >= 0) {
				PLEditor editor = ((PLEditor) UIUtils.getActiveEditor());
				editor.selectAndReveal(startOffset, endOffset-startOffset);
				//editor.selectAndReveal(0,1);
				
			}
		}
	}

	private ITreeContentProvider getContentProvider() {
		return contentProvider;
	}
}