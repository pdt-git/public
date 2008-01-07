package org.cs3.pdt.actions;

import org.eclipse.ltk.core.refactoring.participants.ProcessorBasedRefactoring;
import org.eclipse.ltk.core.refactoring.participants.RefactoringProcessor;

public class ApplyTransformationRefactoring extends ProcessorBasedRefactoring {

	private ApplyTransformationProcessor processor;

	public ApplyTransformationRefactoring(ApplyTransformationProcessor processor) {
		super(processor);
		this.processor = processor;
	}

	@Override
	public RefactoringProcessor getProcessor() {
		return processor;
	}

}
