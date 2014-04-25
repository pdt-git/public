/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others)
 * WWW: http://sewiki.iai.uni-bonn.de/research/pdt/start
 * Mail: pdt@lists.iai.uni-bonn.de
 * Copyright (C): 2004-2012, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms
 * of the Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 ****************************************************************************/

/*
 */
package org.cs3.pdt.internal.actions;

import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.Path;
import org.eclipse.debug.core.DebugPlugin;
import org.eclipse.debug.core.ILaunchConfiguration;
import org.eclipse.debug.core.ILaunchManager;
import org.eclipse.debug.ui.DebugUITools;
import org.eclipse.jface.action.Action;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.ide.FileStoreEditorInput;

public class RunUnitTestAction extends Action {
	
	@Override
	public void run() {
		// get input from active editor
		IEditorInput input = UIUtils.getActiveEditor().getEditorInput();
		if (input == null) {
			Debug.warning("Consult action triggered, but active editor input is null.");
			return;
		}

		if (input instanceof IFileEditorInput) {
			// file from workspace 
			IFileEditorInput fileInput = (IFileEditorInput) input;
			IFile workspaceFile = fileInput.getFile();
			runUnitTest(workspaceFile);
		} else if(input instanceof FileStoreEditorInput) {
			//				// external file
			//				FileStoreEditorInput fileInput = (FileStoreEditorInput) input;
			//				File file = new File(fileInput.getURI());
			//				consultExternalFile(file);
		} else {
			Debug.warning("Consult action triggered, but active editor input is no file.");
			return;
		}
	}


	private void runUnitTest(IFile workspaceFile) {
		final IFile f;
		String fileExtension = workspaceFile.getFileExtension();
		if (fileExtension == null) {
			return;
		}
		if (fileExtension.equals("plt")) {
			f = workspaceFile;
		} else {
			String name = workspaceFile.getName();
			String fullPath = name.substring(0,
					name.length() - fileExtension.length() - 1)
					+ ".plt";
			IFile testFile = workspaceFile.getParent().getFile(
					new Path(fullPath));
			if (testFile != null) {
				f = testFile;
			} else {
				return;
			}
		}
		PrologSession session = null;
		try {
			PrologInterface pif = PDTCommonUtil.getActivePrologInterface();
			session = pif.getSession();
			String prologFileName = PDTUtils.getPrologFileName(f);
			String query = "junitadapter:unit_test(UnitName,Test,'"
					+ prologFileName + "',Line)";
			if (session.queryOnce(query) != null) {
				session.queryOnce("assert(junitadapter:file_to_test('"
						+ prologFileName + "'))");

				ILaunchConfiguration[] launchConfigurations = DebugPlugin
						.getDefault().getLaunchManager()
						.getLaunchConfigurations();
				ILaunchConfiguration plunit = null;
				for (ILaunchConfiguration iLaunchConfiguration : launchConfigurations) {
					if (iLaunchConfiguration.getName().equals("PLUnitTest")) {
						plunit = iLaunchConfiguration;
						break;
					}
				}
				if (plunit != null) {
					DebugUITools.launch(plunit, ILaunchManager.RUN_MODE);

				}
			}
		} catch (Exception e) {
			Debug.report(e);
		} finally {
			if (session != null) {
				session.dispose();
			}
		}
	}
	
}


