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

package org.cs3.pdt.editor.internal.actions;

import static org.cs3.pdt.common.search.SearchConstants.RESULT_KIND_DYNAMIC;
import static org.cs3.pdt.common.search.SearchConstants.RESULT_KIND_FOREIGN;
import static org.cs3.pdt.common.search.SearchConstants.RESULT_KIND_MULTIFILE;
import static org.cs3.prolog.connector.common.QueryUtils.bT;

import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import org.cs3.pdt.common.PDTCommonPredicates;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.pdt.common.metadata.Goal;
import org.cs3.pdt.common.metadata.SourceLocation;
import org.cs3.pdt.connector.util.FileUtils;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.pdt.editor.PDT;
import org.cs3.pdt.editor.internal.editors.PLEditor;
import org.cs3.prolog.connector.common.Debug;
import org.cs3.prolog.connector.common.QueryUtils;
import org.cs3.prolog.connector.process.PrologProcessException;
import org.cs3.prolog.connector.session.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.text.IDocument;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	
	private static final String MESSAGE_EXTERNAL_PREDICATE = "There is no Prolog source code for this predicate (only compiled external language code).";

	private static final String MESSAGE_DYNAMIC_PREDICATE = "There is no Prolog source code for this dynamic predicate.";

	private static final String MESSAGE_UNDEFINED_PREDICATE = "The selected predicate is not defined.";

	public static final String NAME = "Open Primary Definition or Declaration";
	
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

	private static class SourceLocationAndResultKind {
		SourceLocation location;
		String resultKind;

		SourceLocationAndResultKind(SourceLocation location, String resultKind) {
			this.location = location;
			this.resultKind = resultKind;
		}
	}

	private void run_impl(final Goal goal, IFile file) throws CoreException {
		PrologSession session = null;
		try {
			session = PDTCommonUtil.getActivePrologProcess().getSession();
			SourceLocationAndResultKind res = findFirstClauseLocation(goal, session);
			if (res != null) {
				if (res.location != null) {
					PDTCommonUtil.showSourceLocation(res.location);
					if (RESULT_KIND_MULTIFILE.equals(res.resultKind)) {
						new FindDefinitionsActionDelegate(editor).run();
					}
				} else {
					if (RESULT_KIND_DYNAMIC.equals(res.resultKind)) {
						UIUtils.displayMessageDialog(
								editor.getSite().getShell(),
								NAME,//"Dynamic predicate declared in user",
								MESSAGE_DYNAMIC_PREDICATE);
						return;
					} else if (RESULT_KIND_FOREIGN.equals(res.resultKind)) {
						UIUtils.displayMessageDialog(
								editor.getSite().getShell(),
								NAME,//"External language predicate",
								MESSAGE_EXTERNAL_PREDICATE);
						return;
					}
				}
			} else {
				if (!"lgt".equals(file.getFileExtension())) {
					IDocument document = editor.getDocumentProvider().getDocument(editor.getEditorInput());
					int start = UIUtils.physicalToLogicalOffset(document, goal.getStart());
					int end = UIUtils.physicalToLogicalOffset(document, goal.getEnd());
					final List<Map<String, Object>> result = session.queryAll(bT(PDTCommonPredicates.FIND_ALTERNATIVE_PREDICATES,
							QueryUtils.quoteAtom(FileUtils.prologFileName(file)),
							Integer.toString(goal.getLine()),
							Integer.toString(start),
							Integer.toString(end),
							QueryUtils.quoteAtom(goal.getTermString()),
							"ResultKind",
							"RefModule",
							"RefName",
							"RefArity",
							"RefFile",
							"RefLine"));
					if (result.isEmpty()) {
						UIUtils.displayMessageDialog(
								editor.getSite().getShell(),
								NAME,//"Undefined predicate",
								MESSAGE_UNDEFINED_PREDICATE);
						return;
					} else if ("transparent".equals(result.get(0).get("ResultKind"))) {
						if (result.size() == 1) {
							final String fileName = (String) result.get(0).get("RefFile");
							final int line;
							try {
								line = Integer.parseInt((String) result.get(0).get("RefLine"));
								editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
									@Override
									public void run() {
										try {
											PDTCommonUtil.selectInEditor(line, fileName, true);
										} catch (PartInitException e) {
											Debug.report(e);
										}
									}
								});
							} catch (NumberFormatException e) {
							}
						} else {
							editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
								@Override
								public void run() {
									AlternativeDialog alternativeDialog = new AlternativeDialog(editor.getEditorSite().getShell(), result, "The selected predicate " + goal.getSignature() + " has multiple targets.\n" +
											"Select a target and press OK to open it in an editor.");
									alternativeDialog.setBlockOnOpen(false);
									alternativeDialog.open();
								}
							});
						}
					} else {
						editor.getEditorSite().getShell().getDisplay().asyncExec(new Runnable() {
							@Override
							public void run() {
								AlternativeDialog alternativeDialog = new AlternativeDialog(editor.getEditorSite().getShell(), result, "The selected predicate " + goal.getSignature() + " was not found. A list of similar predicates is listed below.\n" +
										"Select a predicate and press OK to open it in an editor.");
								alternativeDialog.setBlockOnOpen(false);
								alternativeDialog.open();
							}
						});
					}
				} else {
					UIUtils.displayMessageDialog(
							editor.getSite().getShell(),
							NAME,//"Undefined predicate",
							MESSAGE_UNDEFINED_PREDICATE);
					return;
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

	private SourceLocationAndResultKind findFirstClauseLocation(Goal goal, PrologSession session) throws PrologProcessException {
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
		
		String term = goal.getTermString();
		String quotedTerm = QueryUtils.quoteAtom(term);

		String query = bT(PDTCommonPredicates.FIND_PRIMARY_DEFINITION_VISIBLE_IN, QueryUtils.quoteAtom(enclFile), goal.getLine(), quotedTerm, "File", "Line", "ResultKind");
		Debug.info("open declaration: " + query);
		Map<String, Object> clause = session.queryOnce(query);
		if (clause == null) {
			return null;
		}
		String resultKind = clause.get("ResultKind").toString();
		if (RESULT_KIND_FOREIGN.equals(resultKind) || RESULT_KIND_DYNAMIC.equals(resultKind)) {
			return new SourceLocationAndResultKind(null, resultKind);
		}
		
		if (clause.get("File") == null) {
			throw new RuntimeException("Cannot resolve file for primary declaration of " + quotedTerm);
		}
		SourceLocation location = new SourceLocation((String) clause.get("File"), false);
		location.setLine(Integer.parseInt((String) clause.get("Line")));
		return new SourceLocationAndResultKind(location, resultKind);
	}
	
	private static class AlternativeDialog extends Dialog {

		private List<Map<String, Object>> alternatives;
		private org.eclipse.swt.widgets.List list;
		private String description;

		protected AlternativeDialog(Shell parentShell, List<Map<String, Object>> alternatives, String description) {
			super(parentShell);
			setShellStyle(getShellStyle() | SWT.RESIZE);
			this.alternatives = alternatives;
			this.description = description;
		}
		
		@Override
		protected Control createDialogArea(Composite parent) {
			Composite composite = (Composite) super.createDialogArea(parent);
			
			Label label = new Label(composite, SWT.WRAP);
			label.setText(description);
			
		    GridData gridData = new GridData();
		    gridData.grabExcessHorizontalSpace = true;
		    gridData.horizontalAlignment = GridData.FILL;
		    gridData.heightHint = convertHeightInCharsToPixels(3);
		    
		    label.setLayoutData(gridData);
			
			list = new org.eclipse.swt.widgets.List(composite, SWT.BORDER);
			for (Map<String, Object> alternative : alternatives) {
				list.add(getTextForPred(alternative));
			}
			list.setSelection(0);
			list.addMouseListener(new MouseAdapter() {
				@Override
				public void mouseDoubleClick(MouseEvent e) {
					if (list.getSelectionIndex() >= 0) {
						AlternativeDialog.this.okPressed();
					}
				}
			});
			
		    gridData = new GridData();
		    gridData.grabExcessHorizontalSpace = true;
		    gridData.horizontalAlignment = GridData.FILL;
		    gridData.grabExcessVerticalSpace = true;
		    gridData.verticalAlignment = GridData.FILL;
		    
		    list.setLayoutData(gridData);

			return composite;
		}
		
		private String getTextForPred(Map<String, Object> predicate) {
			StringBuffer buf = new StringBuffer();
			buf.append(predicate.get("RefModule"));
			buf.append(":");
			buf.append(predicate.get("RefName"));
			buf.append("/");
			buf.append(predicate.get("RefArity"));
			if ("-1".equals(predicate.get("RefLine"))) {
				buf.append(" (no source)");
			}
			return buf.toString();
		}
		
		@Override
		protected void createButtonsForButtonBar(Composite parent) {
			createButton(parent, IDialogConstants.OK_ID, IDialogConstants.OK_LABEL, true);
		}
		
		@Override
		protected void configureShell(Shell newShell) {
			super.configureShell(newShell);
			newShell.setText(NAME);
		}
		
		@Override
		protected Point getInitialSize() {
			return new Point(400, 300);
		}
		
		@Override
		protected void okPressed() {
			int selection = list.getSelectionIndex();
			if (selection >= 0) {
				Map<String, Object> predicate = alternatives.get(selection);
				if (!"-1".equals(predicate.get("RefLine"))) {
					try {
						PDTCommonUtil.selectInEditor(Integer.parseInt(predicate.get("RefLine").toString()), predicate.get("RefFile").toString(), true);
					} catch (PartInitException e) {
						Debug.report(e);
					} catch (NumberFormatException e) {
						Debug.report(e);
					}
				}
			}
			super.okPressed();
		}

	}
	
}



