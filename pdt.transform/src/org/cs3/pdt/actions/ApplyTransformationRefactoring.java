/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

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


