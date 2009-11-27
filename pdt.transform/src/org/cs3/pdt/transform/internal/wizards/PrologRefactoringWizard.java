package org.cs3.pdt.transform.internal.wizards;

import org.cs3.pdt.transform.internal.PrologRefactoring;
import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;

public class PrologRefactoringWizard extends RefactoringWizard{

	private PrologRefactoringInfo info;

	public PrologRefactoringWizard(PrologRefactoring refactoring,PrologRefactoringInfo info) {
		super(refactoring, WIZARD_BASED_USER_INTERFACE);
		this.info=info;
	}

	@Override
	protected void addUserInputPages() {
		addPage(new PrologRefactoringInputPage(info));		
	}

}
