package org.cs3.pdt.transform.handlers;

import org.cs3.pdt.transform.PDTTransformationsPlugin;
import org.cs3.pdt.transform.internal.PrologRefactoring;
import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.pdt.transform.internal.PrologRefactoringProcessor;
import org.cs3.pdt.transform.internal.RenameFileInfo;
import org.cs3.pdt.transform.internal.wizards.PrologRefactoringWizard;
import org.cs3.pl.common.Util;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.commands.IHandlerListener;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;

public class ExecuteRefactoringHandler implements IHandler {

	public void addHandlerListener(IHandlerListener handlerListener) {
		// TODO Auto-generated method stub

	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
	/*	String refactoringId= event.getParameter("pdt.transform.executeRefactoring.refactoring");
		PDTTransformationsPlugin.getDefault().getPrologRefactoringDescriptor(refactoringId);
		//event.getApplicationContext()
		PrologRefactoringInfo info = new RenameFileInfo(getSelectedFile(),getCurrentPrologInterface());
		PrologRefactoringProcessor processor = new PrologRefactoringProcessor(info);
		PrologRefactoring refac = new PrologRefactoring(processor);
		PrologRefactoringWizard wizard = new PrologRefactoringWizard(refac,info);
		RefactoringWizardOpenOperation op 
	      = new RefactoringWizardOpenOperation( wizard );
	    try {
	      String titleForFailedChecks = ""; //$NON-NLS-1$
	      op.run( editor.getSite().getShell(), titleForFailedChecks );
	    } catch( final InterruptedException irex ) {
	      // operation was cancelled
	    }
		*/
		return null;
	}

	public boolean isEnabled() {
		// TODO Auto-generated method stub
		return true;
	}

	public boolean isHandled() {
		// TODO Auto-generated method stub
		return true;
	}

	public void removeHandlerListener(IHandlerListener handlerListener) {
		// TODO Auto-generated method stub
		
	}

}
