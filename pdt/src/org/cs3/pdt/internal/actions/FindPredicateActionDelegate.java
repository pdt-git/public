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

import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Clause;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.IMetaInfoProvider;
import org.cs3.pl.metadata.Predicate;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	private ITextEditor editor;

	

	/**
	 *  
	 */
	public FindPredicateActionDelegate(ITextEditor editor) {
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),
				FindPredicateActionDelegate.class.getName(), editor); //$NON-NLS-1$
		this.editor = editor;

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run() {
		try {
			final Goal data = ((PLEditor) editor)
					.getSelectedPrologElement();
			if (data == null) {
				UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
						"PDT Plugin",
						"Cannot locate a predicate at the specified location.");
				return;
			}
			final IFile file = UIUtils.getFileInActiveEditor();
			
			
			Job j = new Job("Searching predicate definition") {
				protected IStatus run(IProgressMonitor monitor) {
					try {
						monitor.beginTask("searching...",
								IProgressMonitor.UNKNOWN);
						run_impl(data,file);

					} catch (Throwable e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

				
			};
			j.schedule();
		} catch (Throwable t) {
			Debug.report(t);
		}

	}

	public void dispose() {
	}

	private void run_impl(Goal goal, IFile file) {
		IPrologProject plprj;
		try {
			plprj = (IPrologProject) file.getProject().getNature(PDTCore.NATURE_ID);
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		IMetaInfoProvider mip = plprj.getMetaInfoProvider();
		Predicate[] predicates = mip.findPredicates(goal);
		//FIXME: what about alternatives?
		if(predicates==null||predicates.length==0){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Can't find predicate: " + goal.getName()
							+ "/" //$NON-NLS-1$
							+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress.");
			return;
		}
		if(predicates.length>1){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Note: I found more than one predicate matching the signature \n" 
					+ goal.getName()+"/"+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress. " +
									"For now i will just take you " +
									"to the first match found.");
		}
		Clause[] clauses = mip.findClauses(predicates[0]);
		if(clauses==null || clauses.length==0){
			UIUtils.displayMessageDialog(editor.getEditorSite().getShell(),
					"PDT Plugin", "Can't find clauses for predicate: " + goal.getName()
							+ "/" //$NON-NLS-1$
							+ goal.getArity()
							+ ".\nSorry, Code analysis is still work in progress.");
			return;
		}
		PDTUtils.showSourceLocation(clauses[0].getSourceLocation());
	}
	
	

}