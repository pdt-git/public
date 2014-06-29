package org.cs3.pdt.refactoring;

import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

public class PDTRefactoringWizard extends RefactoringWizard {

	
	
	private RenameWizardPage renamePage;
	private String newText;

	public PDTRefactoringWizard(Refactoring refactoring) {
		super(refactoring, WIZARD_BASED_USER_INTERFACE);
		setForcePreviewReview(true);
		this.renamePage = new RenameWizardPage();
	}

	@Override
	protected void addUserInputPages() {
		addPage(renamePage);
	}

	public String getNewText() {
		return this.newText;
	}

	public void setNewText(String text) {
		this.newText = text;
		
	}
	
	

}
