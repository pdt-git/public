/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

package org.cs3.pdt.internal.actions;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.util.Map;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.metadata.Goal;
import org.cs3.pdt.metadata.SourceLocation;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Shell;
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
		super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI), FindPredicateActionDelegate.class.getName(), editor);
		this.editor = editor;

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	@Override
	public void run() {
		try {
			final Goal goal = ((PLEditor) editor).getSelectedPrologElement();
			Shell shell = editor.getEditorSite().getShell();
			if (goal == null) {

				UIUtils.displayMessageDialog(shell, "PDT Plugin", "Cannot locate a predicate at the specified location.");
				return;
			}
			final IFile file = UIUtils.getFileInActiveEditor();
			if (file == null) {
				// UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(),
				// shell,
				// PDT.ERR_NO_ACTIVE_FILE, PDT.CX_FIND_PREDICATE, null);
			}

			Job j = new Job("Searching predicate definition") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					try {
						monitor.beginTask("searching...", IProgressMonitor.UNKNOWN);

						run_impl(goal, file);

					} catch (Throwable e) {
						Debug.report(e);

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

	private static class SourceLocationAndIsMultifileResult {
		SourceLocation location;
		boolean isMultifileResult;

		SourceLocationAndIsMultifileResult(SourceLocation location, boolean isMultifileResult) {
			this.location = location;
			this.isMultifileResult = isMultifileResult;
		}
	}

	private void run_impl(Goal goal, IFile file) throws CoreException {
		PrologSession session = null;
		try {
			session = PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().getActivePrologInterface().getSession();
			SourceLocationAndIsMultifileResult res = findFirstClauseLocation(goal, session);
			if (res != null) {
				if (res.location != null) {
					PDTUtils.showSourceLocation(res.location);
				}
				if (res.isMultifileResult) {
					new FindDefinitionsActionDelegate(editor).run();
				}
			}
			return;
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null)
				session.dispose();
		}
//		UIUtils.getDisplay().asyncExec(new Runnable() {
//
//			@Override
//			public void run() {
//				MessageBox messageBox = new MessageBox(UIUtils.getActiveShell(), SWT.ICON_WARNING | SWT.OK);
//
//				messageBox.setText("Open Declaration");
//				messageBox.setMessage("Cannot open declaration. No active Prolog Console available for a fallback.");
//				messageBox.open();
//			}
//		});

	}

	private SourceLocationAndIsMultifileResult findFirstClauseLocation(Goal goal, PrologSession session) throws PrologInterfaceException {
		// TODO: Schon im goal definiert. Müsste nur noch dort gesetzt werden:
		String enclFile = UIUtils.getFileFromActiveEditor();
		// TODO: if (enclFile==null) ... Fehlermeldung + Abbruch ...

		// Folgendes liefert bei Prolog-Libraries die falschen Ergebnisse,
		// obwohl das aufgerufene Prädikat das Richtige tut, wenn es direkt
		// in einem Prolog-Prozess aufgerufen wird:
		// if(goal.getModule()==null) {
		// String query = "module_of_file('" + enclFile + "',Module)";
		// String referencedModule = (String)
		// session.queryOnce(query).get("Module");
		// goal.setModule(referencedModule);
		// }
		// In der Klasse DefinitionsSearchQuery funktioniert es aber!

		String module = "_";
		if (goal.getModule() != null)
			module = "'" + goal.getModule() + "'";

		String term = goal.getTermString();
		String quotedTerm = Util.quoteAtom(term);

		String query = bT(PDTCommonPredicates.FIND_PRIMARY_DEFINITION_VISIBLE_IN, Util.quoteAtom(enclFile), quotedTerm, module, "File", "Line", "MultifileResults");
		Debug.info("open declaration: " + query);
		Map<String, Object> clause = session.queryOnce(query);
		if (clause == null) {
			return null;
		}
		if (clause.get("File") == null) {
			throw new RuntimeException("Cannot resolve file for primary declaration of " + quotedTerm);
		}
		SourceLocation location = new SourceLocation((String) clause.get("File"), false);
		location.setLine(Integer.parseInt((String) clause.get("Line")));
		boolean otherLocations = "yes".equalsIgnoreCase(clause.get("MultifileResults").toString());
		return new SourceLocationAndIsMultifileResult(location, otherLocations);
	}

}


