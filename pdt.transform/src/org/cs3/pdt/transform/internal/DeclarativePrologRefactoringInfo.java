package org.cs3.pdt.transform.internal;

import org.cs3.pdt.transform.PrologRefactoringDescriptor;
import org.cs3.pl.common.Option;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IWorkbenchPartReference;

public class DeclarativePrologRefactoringInfo extends PrologRefactoringInfo {
	private PrologRefactoringDescriptor descriptor;
	private PrologInterface pif;
	private Option[] options;
	private String selectionTerm;

	public DeclarativePrologRefactoringInfo(PrologInterface pif,PrologRefactoringDescriptor desc, ISelection selection,IWorkbenchPartReference activePart) {
		this.descriptor=desc;
		this.pif=pif;
		this.options=descriptor.getParameters(selection, activePart);
		this.selectionTerm=descriptor.getSelectionTerm(selection, activePart);
	}
	
	@Override
	public String getHead() {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public String getName() {
		return descriptor.getLabel();
	}

	@Override
	public PrologInterface getPrologInterace() {	
		return pif;
	}

	public Option[] getOptions() {
		return options;
	}

}
