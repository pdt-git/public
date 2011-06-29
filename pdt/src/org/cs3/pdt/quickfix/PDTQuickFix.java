package org.cs3.pdt.quickfix;

import org.cs3.pdt.internal.editors.PLMarkerUtils;
import org.cs3.pdt.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IMarker;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.TextFileChange;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.text.edits.InsertEdit;
import org.eclipse.text.edits.MultiTextEdit;
import org.eclipse.ui.IMarkerResolution;

public class PDTQuickFix implements IMarkerResolution {

	private String label;
	private boolean showWizard;

	public PDTQuickFix(String label) {
		this(label, false);
	}

	public PDTQuickFix(String label, boolean showWizard) {
		this.label = label;
		this.showWizard = showWizard;
		if (showWizard) {
			this.label += " (preview changes)";
		}
	}

	@Override
	public String getLabel() {
		return label;
	}

	@Override
	public void run(IMarker marker) {

		IFile file = (IFile) marker.getResource();
		TextFileChange textFileChange = new TextFileChange(file.getName(), file);
		MultiTextEdit fileChangeRootEdit = new MultiTextEdit();

		// a file change contains a tree of edits, first add the root of them
		textFileChange.setEdit( fileChangeRootEdit );
		int offset;
		try {
			offset = Integer.parseInt(marker.getAttribute(IMarker.CHAR_START).toString());
			InsertEdit quickfix = new InsertEdit(offset, marker.getAttribute(PDTMarker.QUICKFIX_ACTION).toString());

			fileChangeRootEdit.addChild(quickfix);

			if (showWizard){
				final TextFileChange fChange = textFileChange;
				Refactoring ref = new Refactoring() {

					@Override
					public String getName() {
						return "PDT Refactoring";
					}

					@Override
					public Change createChange(IProgressMonitor pm) throws CoreException,
					OperationCanceledException {
						return fChange;
					}

					@Override
					public RefactoringStatus checkInitialConditions(IProgressMonitor pm)
							throws CoreException, OperationCanceledException {
						return  new RefactoringStatus();
					}

					@Override
					public RefactoringStatus checkFinalConditions(IProgressMonitor pm)
							throws CoreException, OperationCanceledException {
						return new RefactoringStatus();
					}
				};
				RefactoringWizard wizard = new RefactoringWizard(ref, RefactoringWizard.WIZARD_BASED_USER_INTERFACE) {

					@Override
					protected void addUserInputPages() {
					}
				};
				Shell shell = UIUtils.getActiveShell();
				RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(wizard);
				try {		
					if (op.run(shell, "") != IDialogConstants.CANCEL_ID){
						// changes are already performed by the dialog
						file.refreshLocal(IResource.DEPTH_INFINITE, null);
						PLMarkerUtils.updateFileMarkers(file);
					}
				} catch (InterruptedException e) {
				}
			} else {
				textFileChange.perform(new NullProgressMonitor());

				file.refreshLocal(IResource.DEPTH_INFINITE, null);
				PLMarkerUtils.updateFileMarkers(file);
			}
		} catch (NumberFormatException e1) {
			e1.printStackTrace();
		} catch (CoreException e1) {
			e1.printStackTrace();
		}
	}

}
