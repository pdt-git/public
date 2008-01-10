package pdt.transform.wizards;

import org.cs3.pdt.actions.PrologRefactoring;
import org.cs3.pdt.actions.PrologRefactoringInfo;
import org.eclipse.ltk.core.refactoring.Refactoring;
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
