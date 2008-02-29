package org.cs3.pdt.transform.handlers;

import org.cs3.pdt.transform.PDTTransformationsPlugin;
import org.cs3.pdt.transform.PrologRefactoringDescriptor;
import org.cs3.pdt.transform.internal.DeclarativePrologRefactoringInfo;
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
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.window.IShellProvider;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.handlers.HandlerUtil;

public class ExecuteRefactoringHandler implements IHandler {

	public void addHandlerListener(IHandlerListener handlerListener) {
		// TODO Auto-generated method stub

	}

	public void dispose() {
		// TODO Auto-generated method stub

	}

	public Object execute(ExecutionEvent event) throws ExecutionException {
		ISelection selection=HandlerUtil.getCurrentSelectionChecked(event);
		IWorkbenchPart activePart = HandlerUtil.getActivePartChecked(event);
		Shell shell = HandlerUtil.getActiveShellChecked(event);
		String refactoringId= event.getParameter("pdt.transform.executeRefactoring.refactoring");
		PrologRefactoringDescriptor desc;
		PrologRefactoringInfo info;
		try {
			desc = PDTTransformationsPlugin.getDefault().getPrologRefactoringDescriptor(refactoringId);
			if(desc==null){
				throw new RuntimeException();
			}
			
			info = new DeclarativePrologRefactoringInfo(desc,selection,activePart);
		} catch (Exception e) {
			throw new ExecutionException("failed to obtain Refactoring Descriptor for id "+refactoringId,e);
		}

		
		PrologRefactoringProcessor processor = new PrologRefactoringProcessor(info);
		PrologRefactoring refac = new PrologRefactoring(processor);
		PrologRefactoringWizard wizard = new PrologRefactoringWizard(refac,info);
		RefactoringWizardOpenOperation op 
	      = new RefactoringWizardOpenOperation( wizard );
	    try {
	      String titleForFailedChecks = ""; //$NON-NLS-1$
	      op.run( shell, titleForFailedChecks );
	    } catch( final InterruptedException irex ) {
	      // operation was canceled
	    }
		
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
