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

package org.cs3.pdt.connector.internal.service;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;

import org.cs3.pdt.connector.service.PDTReloadExecutor;
import org.cs3.pdt.connector.util.UIUtils;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologConnectorPredicates;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

public class DefaultReloadExecutor implements PDTReloadExecutor {
	
	@Override
	public int getPriority() {
		return 0;
	}

	@Override
	public boolean executePDTReload(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		if (files.isEmpty()) {
			monitor.done();
			return true;
		}
		
		String fileList = null;
		try {
			fileList = UIUtils.quotedPrologFileNameList(files);
		} catch (IOException e) {
			Debug.report(e);
			return false;
		}
		pif.queryOnce(bT(PrologConnectorPredicates.PDT_RELOAD, fileList));
		monitor.done();
		return true;
	}
	
}


