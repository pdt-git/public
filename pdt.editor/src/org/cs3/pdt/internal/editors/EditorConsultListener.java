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

package org.cs3.pdt.internal.editors;

import java.util.List;

import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ConsultListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class EditorConsultListener implements ConsultListener {

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		monitor.done();
	}

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files, List<String> allConsultedFiles, IProgressMonitor monitor) throws PrologInterfaceException {
		PLMarkerUtils.addMarkers(pif, allConsultedFiles, monitor);
	}
	
}


