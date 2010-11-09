/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

/*
 */
package org.cs3.pdt.internal.actions;

import java.io.File;
import java.io.IOException;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.runtime.ui.PrologRuntimeUIPlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialogWithToggle;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.PartInitException;

/**
 */
public class ConsultActionDelegate extends QueryConsoleThreadAction implements
		IWorkbenchWindowActionDelegate {

	
	private IWorkbenchWindow window;

	public ConsultActionDelegate() {
		super(null, "consult", "consult action", null);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#dispose()
	 */
	@Override
	public void dispose() {
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchWindowActionDelegate#init(org.eclipse.ui.IWorkbenchWindow)
	 */
	@Override
	public void init(IWorkbenchWindow window) {
		this.window = window;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
	 */
	@Override
	public void run(IAction action) {
		PDTPlugin plugin = PDTPlugin.getDefault();
		IEditorInput input = UIUtils.getActiveEditor().getEditorInput();
		if (input == null) {
			Debug
					.warning("Consult action triggered, but active editor input is null.");
		}
		if (input instanceof IFileEditorInput) {
			IFileEditorInput fileInput = (IFileEditorInput) input;
			try {
				File file = fileInput.getFile().getLocation().toFile()
						.getCanonicalFile();
				plugin.getWorkbench().getActiveWorkbenchWindow()
						.getActivePage().showView(PDTConsole.CONSOLE_VIEW_ID);
				
				checkPif();
				

				setQuery("consult('" + Util.prologFileName(file) + "')");

				run();
			} catch (IOException e) {
				Debug.report(e);
//				UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(),
//						window.getShell(), PDT.ERR_FILENAME_CONVERSION_PROBLEM, PDT.CX_CONSULT, e);
			} catch (PartInitException e) {
				Debug.report(e);
//				UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(),
//						window.getShell(), PDT.ERR_WORKBENCH_UI_PROBLEM, PDT.CX_CONSULT, e);
			} catch (OperationCanceledException e) {
				return;
			} catch (PrologInterfaceException e) {
				Debug.report(e);
//				UIUtils.logAndDisplayError(PDTPlugin.getDefault().getErrorMessageProvider(),
//						window.getShell(), PDT.ERR_PIF, PDT.CX_CONSULT, e);
			}

		} else {
			Debug
					.warning("Consult action triggered, but active editor input is no file.");
		}
	}

	private void checkPif() throws OperationCanceledException, PrologInterfaceException{
		PrologInterface pif = PDTUtils.getActiveConsolePif();
		if (pif == null) {
			// boring. nothing to check.
			return;
		}
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		String consolePifKey = reg.getKey(pif);
		String projectPifKey = null;
		IFile file = UIUtils.getFileInActiveEditor();
		IPrologProject plProject = null;
		IProject project = null;
		if (file != null) {
			project = file.getProject();
		}
		try {
			if (project != null && project.hasNature(PDTCore.NATURE_ID)) {
				plProject = (IPrologProject) project
						.getNature(PDTCore.NATURE_ID);
			}
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		if (plProject == null) {
			return;
		}
		projectPifKey = plProject.getRuntimeSubscription().getPifKey();
		if (!projectPifKey.equals(consolePifKey)) {
			String dialogTitle = "Switch to default runtime?";
			String dialogMessage = "The project "
					+ project.getName()
					+ " uses "
					+ projectPifKey
					+ " as its default runtime. However the active console view"
					+ " is connected to " + consolePifKey + ".\n"
					+ "Do you want to switch to the default runtime ("
					+ projectPifKey + ") before consulting?";
			String toggleMessage = null;
			PDTPlugin plugin = PDTPlugin.getDefault();
			String key = PDT.PREF_SWITCH_TO_DEFAULT_PIF;
			String pref = plugin.getPreferenceValue(key,
					MessageDialogWithToggle.PROMPT);
			boolean toggleState = false;
			boolean shouldSwitchPif = MessageDialogWithToggle.ALWAYS
					.equals(pref);
			IPreferenceStore store = plugin.getPreferenceStore();
			if (MessageDialogWithToggle.PROMPT.equals(pref)) {
				MessageDialogWithToggle toggle = MessageDialogWithToggle
						.openYesNoCancelQuestion(window.getShell(), dialogTitle,
								dialogMessage, toggleMessage, toggleState,
								store, key);
				if(toggle.getReturnCode()==IDialogConstants.CANCEL_ID){
					throw new OperationCanceledException();
				}
				shouldSwitchPif = IDialogConstants.YES_ID == toggle
						.getReturnCode();
				
			}
			if (shouldSwitchPif) {

				
					pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(
							projectPifKey);
				
				PrologConsolePlugin.getDefault().getPrologConsoleService()
						.getActivePrologConsole().setPrologInterface(pif);

			}
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
	 *      org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void selectionChanged(IAction action, ISelection selection) {
	}

}
