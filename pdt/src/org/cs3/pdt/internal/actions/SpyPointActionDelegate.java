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

package org.cs3.pdt.internal.actions;

import java.util.Hashtable;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class SpyPointActionDelegate extends TextEditorAction {
	/**
	 * 
	 */
	public SpyPointActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),
				SpyPointActionDelegate.class.getName(), editor); //$NON-NLS-1$
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */

	Hashtable spypred = new Hashtable();

	public void run() {
		PDTPlugin plugin = PDTPlugin.getDefault();
		UIUtils.getDisplay().asyncExec(new Runnable() {
			public void run() {

				PLEditor editor = (PLEditor) UIUtils.getActiveEditor();
				IFileEditorInput editorInput = (IFileEditorInput) editor
						.getEditorInput();
				IPrologProject plProject;
				try {
					plProject = (IPrologProject) editorInput.getFile()
							.getProject().getNature(PDTCore.NATURE_ID);
				} catch (CoreException e) {
					Debug.report(e);
					throw new RuntimeException(e);
				}
				PrologSession session = null;
				/*
				 * FIXME which PIF to use? actualy we should not decide here. We
				 * are setting a property of the source file, not of any
				 * runtime. this should be anotated using resource properties or
				 * something similar and the consult action should care about
				 * this. I am creating an issue, so we do not forget about this
				 * problem.
				 */
				try {
					session = PDTUtils.getActiveConsolePif().getSession();
				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				}
				String pred;

				Goal data;
				try {
					data = editor.getSelectedPrologElement();
				} catch (BadLocationException e2) {
					Debug.report(e2);
					return;
				}
				if (data == null) {
					MessageDialog
							.openInformation(editor.getEditorSite().getShell(),
									"PDT Plugin",
									"Cannot locate a predicate at the specified location.");
					return;

				}
				Predicate[] p=null;
				try {
					p = plProject.getMetaInfoProvider().findPredicates(
							data);
				} catch (PrologInterfaceException e) {
					Debug.rethrow( e);
				}
				// FIXME what about alternatives?

				if (p == null || p.length == 0) {
					MessageDialog.openInformation(editor.getEditorSite()
							.getShell(), "PDT Plugin",
							"Cannot find predicate: " + data //$NON-NLS-2$
									+ "."); //$NON-NLS-1$
					return;

				}
				if (p.length > 1) {
					UIUtils
							.displayMessageDialog(
									UIUtils.getActiveEditor().getEditorSite()
											.getShell(),
									"PDT Plugin",
									"Note: I found more than one predicate matching the signature \n"
											+ data.getName()
											+ "/"
											+ data.getArity()
											+ ".\nSorry, Code analysis is still work in progress. "
											+ "For now i will ignore all but the first match.");
				}
				pred = p[0].getSignature();

				if (spypred.get(pred) != null) {
					try {
						session.queryOnce("nospy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
					} catch (PrologException e1) {
						Debug.report(e1);
						return;
					} catch (PrologInterfaceException e) {
						Debug.report(e);
						return;
					}
					spypred.remove(pred);
				} else {
					try {
						session.queryOnce("spy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
					} catch (PrologException e1) {
						Debug.report(e1);
						return;
					} catch (PrologInterfaceException e) {
						Debug.report(e);
						return;
					}
					spypred.put(pred, pred);
				}

			}
		});
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose() {
	}

}