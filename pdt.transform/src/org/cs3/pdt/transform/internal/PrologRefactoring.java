package org.cs3.pdt.transform.internal;

import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;

public class PrologRefactoring extends ProcessorBasedRefactoring  {

	private PrologRefactoringProcessor processor;
	public PrologRefactoring(PrologRefactoringProcessor processor) {
		super(processor);
		this.processor = processor;
	}

	@Override
	public RefactoringProcessor getProcessor() {
		return processor;
	}

	
}
