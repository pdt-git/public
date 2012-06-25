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
import java.util.List;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.core.PDTCoreUtils;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.internal.editors.PLMarkerUtils;
import org.cs3.pdt.internal.editors.breakpoints.PDTBreakpointHandler;
import org.cs3.pdt.internal.views.lightweightOutline.NonNaturePrologOutline;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.QueryUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.FileStoreEditorInput;
import org.eclipse.ui.views.contentoutline.ContentOutlinePage;

public class ConsultAction extends QueryConsoleThreadAction {
	
	public ConsultAction() {
		super(null, "consult", "consult action", null);
	}
	
	public void consultWorkspaceFiles(final List<IFile> filesToConsult) {

		Job j = new Job("consult files from workspace") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (PDTUtils.checkForActivePif(true)) {
					// for workspace files, create markers & do consult
					try {
						monitor.beginTask("consult " + filesToConsult.size() + " files", 3);
						PrologInterface pif = PDTUtils.getActiveConsolePif();
						if( pif != null) {
							PDTBreakpointHandler.getInstance().backupMarkers(null, null);
							monitor.subTask("activate warning and error tracing");
							activateWarningAndErrorTracing(pif, new SubProgressMonitor(monitor, 1));
							monitor.subTask("load file");
							doConsult(filesToConsult, new SubProgressMonitor(monitor, 1));
							monitor.done();
							monitor.beginTask("update markers", filesToConsult.size());
//							monitor.subTask("update markers");
							for (IFile f : filesToConsult) {
								PLMarkerUtils.addMarkers(f, new SubProgressMonitor(monitor, 1));
							}
						}
					} catch (PrologInterfaceException e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
				}
				return Status.OK_STATUS;
			}
		};
//		TODO: find a good rule
//		j.setRule();
		j.setRule(ResourcesPlugin.getWorkspace().getRoot());
		j.schedule();
	}
	
	private void doConsult(List<IFile> filesToConsult, IProgressMonitor monitor) {
		monitor.beginTask("consult files", 1);
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				try {
					PDTPlugin.getActivePage().showView(PDTConsole.CONSOLE_VIEW_ID);
				} catch (PartInitException e) {
					Debug.report(e);
				}
			}
		});
		StringBuffer buf = new StringBuffer();
		boolean first = true;
		for (IFile f : filesToConsult) {
			if (first) {
				first = false;
			} else {
				buf.append(", ");
			}
			try {
				buf.append(Util.quoteAtom(Util.prologFileName(f.getLocation().toFile().getCanonicalFile())));
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		setQuery(QueryUtils.buildTerm("pdt_reload", "[" + buf.toString()  + "]"));
		run();
		monitor.done();
	}
		
	public void consultWorkspaceFile(final IFile f) {
		Job j = new Job("consult file from workspace") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				if (PDTUtils.checkForActivePif(true)) {
					// for workspace files, create markers & do consult
					try {
						monitor.beginTask("consult " + f.getName(), 3);
						PrologInterface pif = PDTUtils.getActiveConsolePif();
						if( PDTCoreUtils.getPrologProject(f) == null && pif != null) {
							String prologFileName = Util.prologFileName(f.getLocation().toFile().getCanonicalFile());
							PDTBreakpointHandler.getInstance().backupMarkers(null, null);
							monitor.subTask("activate warning and error tracing");
							activateWarningAndErrorTracing(pif, new SubProgressMonitor(monitor, 1));
							monitor.subTask("load file");
							doConsult(prologFileName, new SubProgressMonitor(monitor, 1));
							monitor.subTask("update markers");
							PLMarkerUtils.addMarkers(f, new SubProgressMonitor(monitor, 1));
						}
					} catch (IOException e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} catch (CoreException e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} catch (PrologInterfaceException e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
				}
				return Status.OK_STATUS;
			}
		};
		j.setRule(f);
		j.schedule();
	}

	private void activateWarningAndErrorTracing(PrologInterface pif, IProgressMonitor monitor) throws PrologInterfaceException {
		monitor.beginTask("activate warning and error tracing", 1);
//		pif.queryOnce("activate_warning_and_error_tracing");
		monitor.done();
	}
	
	public void consultExternalFile(final File f) {

		Job j = new Job("consult file from workspace") {
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("consult external file", 1);
				// for external files, only do the consult
				String prologFileName = Util.prologFileName(f);
				doConsult(prologFileName, new SubProgressMonitor(monitor, 1));
				monitor.done();
				return Status.OK_STATUS;
			}
		};
		j.setRule(null);
		j.schedule();
	}
	
	private void doConsult(String prologFilename, IProgressMonitor monitor) {
		monitor.beginTask("consult file", 1);
		Display.getDefault().asyncExec(new Runnable() {
			@Override
			public void run() {
				try {
					PDTPlugin.getActivePage().showView(PDTConsole.CONSOLE_VIEW_ID);
				} catch (PartInitException e) {
					Debug.report(e);
				}
			}
		});
		
		setQuery(QueryUtils.buildTerm("pdt_reload", Util.quoteAtom(prologFilename)));
		run();
		monitor.done();
	}

	public void consultFromActiveEditor() {
		if (PDTUtils.checkForActivePif(true)) {
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
				consultWorkspaceFile(workspaceFile);
			} else if(input instanceof FileStoreEditorInput) {
				// external file
				FileStoreEditorInput fileInput = (FileStoreEditorInput) input;
				File file = new File(fileInput.getURI());
				consultExternalFile(file);
			} else {
				Debug.warning("Consult action triggered, but active editor input is no file.");
				return;
			}
		}
	}
	
	// only important for projects with pdt nature
//	private void checkPif() throws OperationCanceledException, PrologInterfaceException{
//		PrologInterface pif = PDTUtils.getActiveConsolePif();
//		if (pif == null) {
//			// boring. nothing to check.
//			return;
//		}
//		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
//		String consolePifKey = reg.getKey(pif);
//		String projectPifKey = null;
//		IFile file = UIUtils.getFileInActiveEditor();
//		IPrologProject plProject = null;
//		IProject project = null;
//		if (file != null) {
//			project = file.getProject();
//		}
//		try {
//			if (project != null && project.hasNature(PDTCore.NATURE_ID)) {
//				plProject = (IPrologProject) project
//						.getNature(PDTCore.NATURE_ID);
//			}
//		} catch (CoreException e) {
//			Debug.report(e);
//			throw new RuntimeException(e);
//		}
//		if (plProject == null) {
//			return;
//		}
//		projectPifKey = plProject.getRuntimeSubscription().getPifKey();
//		if (!projectPifKey.equals(consolePifKey)) {
//			String dialogTitle = "Switch to default runtime?";
//			String dialogMessage = "The project "
//					+ project.getName()
//					+ " uses "
//					+ projectPifKey
//					+ " as its default runtime. However the active console view"
//					+ " is connected to " + consolePifKey + ".\n"
//					+ "Do you want to switch to the default runtime ("
//					+ projectPifKey + ") before consulting?";
//			String toggleMessage = null;
//			PDTPlugin plugin = PDTPlugin.getDefault();
//			String key = PDT.PREF_SWITCH_TO_DEFAULT_PIF;
//			String pref = plugin.getPreferenceValue(key,
//					MessageDialogWithToggle.PROMPT);
//			boolean toggleState = false;
//			boolean shouldSwitchPif = MessageDialogWithToggle.ALWAYS
//					.equals(pref);
//			IPreferenceStore store = plugin.getPreferenceStore();
//			if (MessageDialogWithToggle.PROMPT.equals(pref)) {
//				MessageDialogWithToggle toggle = MessageDialogWithToggle
//						.openYesNoCancelQuestion(window == null ? null : window.getShell(), dialogTitle,
//								dialogMessage, toggleMessage, toggleState,
//								store, key);
//				if(toggle.getReturnCode()==IDialogConstants.CANCEL_ID){
//					throw new OperationCanceledException();
//				}
//				shouldSwitchPif = IDialogConstants.YES_ID == toggle
//						.getReturnCode();
//				
//			}
//			if (shouldSwitchPif) {
//
//				
//					pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(
//							projectPifKey);
//				
//				PrologConsolePlugin.getDefault().getPrologConsoleService()
//						.getActivePrologConsole().setPrologInterface(pif);
//
//			}
//		}
//	}
	
	
	@Override
	public void done(IJobChangeEvent event) {
		PDTPlugin plugin = PDTPlugin.getDefault();
		plugin.getWorkbench().getDisplay().asyncExec(new Runnable() {
			@Override
			public void run() {
				IEditorPart editor = UIUtils.getActiveEditor();

				if ((editor == null) || !(editor instanceof PLEditor)) {
					return;
				}
				PLEditor pleditor = (PLEditor)editor;
				ContentOutlinePage outlinePage = pleditor.getOutlinePage();
				if ((outlinePage != null) && (outlinePage instanceof NonNaturePrologOutline)){
					NonNaturePrologOutline prologOutlinePage = ((NonNaturePrologOutline)outlinePage);
					prologOutlinePage.setInput(pleditor.getEditorInput());
				}
			}
		} );
	}


	


}
