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

import org.cs3.pdt.transform.internal.PrologRefactoringInfo;
import org.cs3.prolog.common.Option;
import org.cs3.prolog.load.PrologLibraryManager;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.session.PrologSession;

public class ApplyTransformationInfo extends PrologRefactoringInfo{

	private final PrologInterface pif;

	public ApplyTransformationInfo(PrologInterface pif) {
		this.pif = pif;
	}

	public PrologInterface getPrologInterface() {
		return pif;
	}

	@Override
	public void configure(PrologLibraryManager libman, PrologSession s)
			throws PrologInterfaceException {
	}

	@Override
	public String getName() {
		return null;
	}

	@Override
	public String getParameterTerm() {
		return null;
	}

	@Override
	public PrologInterface getPrologInterace() {
		return null;
	}

	@Override
	public String getRefactoringId() {
		return null;
	}

	@Override
	public String getSelectionTerm() {
		return null;
	}

	@Override
	public Option[] getOptions() {
		return null;
	}

}


