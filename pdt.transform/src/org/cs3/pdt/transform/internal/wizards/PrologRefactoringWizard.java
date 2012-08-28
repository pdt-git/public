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


