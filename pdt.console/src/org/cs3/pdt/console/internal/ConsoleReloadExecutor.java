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

package org.cs3.pdt.console.internal;

import static org.cs3.prolog.common.QueryUtils.bT;

import java.io.IOException;
import java.util.List;

import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.PrologConnectorPredicates;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.PDTReloadExecutor;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;

public class ConsoleReloadExecutor implements PDTReloadExecutor {
	
	@Override
	public int getPriority() {
		return 1000;
	}
	
	@Override
	public boolean executePDTReload(PrologInterface pif, List<IFile> files, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("", 1);
		if (files.isEmpty()) {
			monitor.done();
			return true;
		}
		
		try {
			boolean first = true;
			StringBuffer buffer = new StringBuffer("[");
			for (IFile f : files) {
				if (first) {
					first = false;
				} else {
					buffer.append(", ");
				}
				try {
					buffer.append(Util.quoteAtom(Util.prologFileName(f)));
				} catch (IOException e) {
					Debug.report(e);
					return false;
				}
			};
			buffer.append("]");
			String query = bT(PrologConnectorPredicates.PDT_RELOAD, buffer.toString());
			return executeQueryOnConsole(pif, query);
		} finally {
			monitor.done();
		}
	}
	
	private boolean executeQueryOnConsole(PrologInterface pif, String query) {
		PrologConsole activePrologConsole = PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole();
		if (activePrologConsole == null) {
			return false;
		}
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				IWorkbenchPage activePage = UIUtils.getActivePage();
				if (activePage != null) {
					try {
						activePage.showView(PDTConsole.CONSOLE_VIEW_ID);
					} catch (PartInitException e) {
						Debug.report(e);
					}
				}
			}
		});
		PrologInterface activeConsolePif = activePrologConsole.getPrologInterface();
		if (activeConsolePif == null || !activeConsolePif.equals(pif)) {
			activePrologConsole.setPrologInterface(pif);
			try {
				Thread.sleep(200);
			} catch (InterruptedException e) {
			}
		}
		if (!pif.isUp()) {
			try {
				pif.start();
				activePrologConsole.ensureConnectionForCurrentPrologInterface();
			} catch (PrologInterfaceException e) {
				Debug.report(e);
				return false;
			}
		}
		ConsoleModel model = activePrologConsole.getModel();
		model.setLineBuffer(" ");
		model.commitLineBuffer();
		if (query.endsWith(".")) {
			model.setLineBuffer(query);
		} else {
			model.setLineBuffer(query + ".");
		}
		model.commitLineBuffer();
		return true;
	}

}


