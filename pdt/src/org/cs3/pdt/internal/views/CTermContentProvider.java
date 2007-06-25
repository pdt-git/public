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

package org.cs3.pdt.internal.views;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IFileEditorInput;

public class CTermContentProvider implements ITreeContentProvider,
		PrologFileContentModelListener {

	private TreeViewer viewer;

	private PrologFileContentModel backend;

	

	public CTermContentProvider(Viewer outline, PrologFileContentModel backend) {
		this.viewer = (TreeViewer) outline;
		this.backend = backend;
		this.backend.addPrologFileContentModelListener(this);

	}

	public Object[] getChildren(Object parentElement) {
		try {
			
			return backend.getChildren(parentElement);
		} catch (PrologInterfaceException e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), viewer.getControl().getShell(),
					PDT.ERR_PIF, PDT.CX_OUTLINE, e);
			return new Object[0];
		}
	}

	public Object getParent(Object element) {
		return null;
	}

	public boolean hasChildren(Object parentElement) {
		// XXX a hack.
		if (parentElement instanceof DirectiveNode
				|| parentElement instanceof ClauseNode) {
			ViewerFilter[] filters = viewer.getFilters();
			for (int i = 0; i < filters.length; i++) {
				if (filters[i] instanceof HideSubtermsFilter) {
					return false;
				}
			}
		}
		return backend.hasChildren(parentElement);

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
	 */
	public Object[] getElements(Object inputElement) {
		return getChildren(inputElement);
	}

	public void dispose() {
		;
	}

	/**
	 * 
	 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(Viewer,
	 *      Object, Object)
	 */
	public void inputChanged(final Viewer viewer, Object oldInput, Object input) {
		
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

			if (plProject != null) {
				backend.setPif(plProject.getMetadataPrologInterface(),plProject.getMetaDataEventDispatcher());
			} else {
				backend.setPif(null,null);
			}

			backend.setInput(input);
			viewer.refresh();

		} catch (Exception e) {
			Debug.report(e);
			UIUtils.logAndDisplayError(PDTPlugin.getDefault()
					.getErrorMessageProvider(), viewer.getControl().getShell(),
					PDT.ERR_PIF, PDT.CX_OUTLINE, e);
		}
	}

	
	public void childrenAdded(final PrologFileContentModelEvent e) {
		if (viewer == null || viewer.getControl().isDisposed()) {
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			Debug.debug("outline enqueue on ui thread: add "
					+ Util.prettyPrint(e.children));
			display.asyncExec(new Runnable() {
				public void run() {
					childrenAdded(e);
				}
			});
			return;
		}

		Debug.debug("outline add (parent=" + e.parent + "): "
				+ Util.prettyPrint(e.children));
		viewer.add(e.parent, e.children);

	}

	public void childrenChanged(final PrologFileContentModelEvent e) {
		if (viewer == null || viewer.getControl().isDisposed()) {
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			Debug.debug("outline enqueue on ui thread: update "
					+ Util.prettyPrint(e.children));
			display.asyncExec(new Runnable() {
				public void run() {
					childrenChanged(e);
				}
			});
			return;
		}
		Debug.debug("outline update " + Util.prettyPrint(e.children));
		viewer.update(e.children, null);
	}

	public void childrenRemoved(final PrologFileContentModelEvent e) {
		if (viewer == null || viewer.getControl().isDisposed()) {
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			Debug.debug("outline enqueue on ui thread: remove "
					+ Util.prettyPrint(e.children));
			display.asyncExec(new Runnable() {
				public void run() {
					childrenRemoved(e);
				}
			});
			return;
		}
		Debug.debug("outline remove " + Util.prettyPrint(e.children));
		viewer.remove(e.children);

	}

	public void contentModelChanged(final PrologFileContentModelEvent e) {
		if (viewer == null || viewer.getControl().isDisposed() || e.isObsolet()) {
			return;
		}
		Display display = viewer.getControl().getDisplay();
		if (Display.getCurrent() != display) {
			Debug
					.debug("outline enqueue on ui thread: refresh (contentModelChanged) "
							+ Util.prettyPrint(e.children));
			display.asyncExec(new Runnable() {
				public void run() {
					contentModelChanged(e);
				}
			});
			return;
		}
		Debug.debug("outline refresh (contentModelChanged)");
		
		viewer.refresh();

	}

}
