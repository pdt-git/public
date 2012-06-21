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

package org.cs3.pdt.console.internal.views;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.cs3.pdt.console.ConsoleModel;
import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.DefaultPrologConsoleService;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.console.internal.loadfile.GenerateLoadFileWizard;
import org.cs3.pdt.console.internal.preferences.PreferencePageMain;
import org.cs3.pdt.console.internal.views.ConsoleViewer.SavedState;
import org.cs3.prolog.common.Util;
import org.cs3.prolog.common.logging.Debug;
import org.cs3.prolog.connector.DefaultSubscription;
import org.cs3.prolog.connector.PrologInterfaceRegistry;
import org.cs3.prolog.connector.PrologRuntimePlugin;
import org.cs3.prolog.connector.Subscription;
import org.cs3.prolog.connector.ui.PrologContextTracker;
import org.cs3.prolog.connector.ui.PrologContextTrackerEvent;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.lifecycle.LifeCycleHook;
import org.cs3.prolog.load.FileSearchPathConfigurator;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ActivePrologInterfaceListener;
import org.cs3.prolog.session.PrologSession;
import org.cs3.prolog.ui.util.EclipsePreferenceProvider;
import org.cs3.prolog.ui.util.UIUtils;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IKeyBindingService;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;

public class PrologConsoleView extends ViewPart implements LifeCycleHook, PrologConsole, ActivePrologInterfaceListener {

	private static final String DEFAULT_CONSOLE = "Default Console";
	private static final String KILLABLE = "killable";

	private final class ClearAction extends Action {
		private ClearAction(String text, String tooltip, ImageDescriptor image) {
			super(text, image);
			setToolTipText(tooltip);
		}

		@Override
		public void run() {
			getViewer().clearOutput();
		}
	}

	private abstract class PasteAction extends Action {
		public PasteAction(String text, String tooltip, ImageDescriptor icon) {
			super(text, icon);

			setToolTipText(tooltip);
		}

		protected abstract String getTextToInsert();

		@Override
		public void run() {
			try {

				UIJob j = new UIJob(getToolTipText()) {

					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							PrologConsole c = getConsole();
							int caretOffset = c.getCaretOffset();
							int offsetInLineBuffer = caretOffset
									- c.getStartOfInput();
							ConsoleModel model = c.getModel();
							String lineBuffer = model.getLineBuffer();
							if (offsetInLineBuffer < 0) {
								offsetInLineBuffer=lineBuffer.length();
								caretOffset=c.getStartOfInput()+lineBuffer.length();
							}


							String textToInsert = getTextToInsert();
							if (textToInsert == null) {
								return Status.OK_STATUS;
							}
							lineBuffer = lineBuffer.substring(0,
									offsetInLineBuffer)
									+ textToInsert
									+ lineBuffer.substring(offsetInLineBuffer);

							model.setLineBuffer(lineBuffer);
							c.setCaretOffset(caretOffset
									+ textToInsert.length());

						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

					private PrologConsole getConsole() {
						return PrologConsoleView.this;
					}

				};

				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}
		}

	}

	private final class RestartAction extends Action {
		@Override
		public void run() {
			try {

				Job j = new Job("Restarting the PrologInterface") {

					@Override
					public IStatus run(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...", 2);

							try {
								if (getPrologInterface() != null) {
									getPrologInterface().stop();
									monitor.worked(1);
								}
								// setPrologInterface(getEditorPrologInterface());
							} finally {
								if (getPrologInterface() != null) {
									if (!getPrologInterface().isDown()){
										getPrologInterface().reset();
										Thread.sleep(1000);
									}
									getPrologInterface().initOptions(new EclipsePreferenceProvider(PrologRuntimeUIPlugin.getDefault()));
									
									final String reconsultFiles = PrologConsolePlugin.getDefault().getPreferenceValue(PDTConsole.PREF_RECONSULT_ON_RESTART, PDTConsole.RECONSULT_NONE);
									
									if (reconsultFiles.equals(PDTConsole.RECONSULT_NONE)) {
										getPrologInterface().clearConsultedFiles();
									} else {

										getPrologInterface().addLifeCycleHook(new LifeCycleHook() {

											@Override
											public void setData(Object data) {}

											@Override
											public void onInit(PrologInterface pif, PrologSession initSession)
													throws PrologInterfaceException {}

											@Override
											public void onError(PrologInterface pif) {}

											@Override
											public void lateInit(PrologInterface pif) {}

											@Override
											public void beforeShutdown(PrologInterface pif, PrologSession session)
													throws PrologInterfaceException {}

											@Override
											public void afterInit(PrologInterface pif) throws PrologInterfaceException {
												pif.reconsultFiles(reconsultFiles.equals(PDTConsole.RECONSULT_ENTRY));
												pif.removeLifeCycleHook(HOOK_ID+"_");

											}
										}, HOOK_ID + "_", new String[0]);
									}
									getPrologInterface().start();

									Display.getDefault().asyncExec(new Runnable() {
										@Override
										public void run() {
											getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
										}
									});
								}
							}
						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

				};
				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}

		}

		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.RESTART);
		}

		@Override
		public String getToolTipText() {
			return "Restart process";
		}

		@Override
		public String getText() {
			return "restart";
		}
	}

	private final class KillAction extends Action {

		@Override
		public void run() {
			
			boolean answer = MessageDialog.openQuestion(PrologConsoleView.this.getViewSite().getShell(), "Kill process", "Are you sure you want to kill the process? This will remove all breakpoints and will delete the list of consulted files");
			
			if (answer) {
				try {

					Job j = new UIJob("Stopping the PrologInterface") {

						@Override
						public IStatus runInUIThread(IProgressMonitor monitor) {
							try {
								monitor.beginTask("initializing...",
										IProgressMonitor.UNKNOWN);

								PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();


								PrologInterface oldPif = getPrologInterface();
								if (oldPif != null) {
									String currentKey = registry.getKey(oldPif);

									if (!currentKey.equals(DEFAULT_CONSOLE)) {
										setPrologInterface(registry.getPrologInterface(DEFAULT_CONSOLE));
									}

									oldPif.clearConsultedFiles();
									oldPif.stop();

									if (!currentKey.equals(DEFAULT_CONSOLE) && "true".equals(oldPif.getAttribute(KILLABLE))) {
										Set<Subscription> subscriptionsForPif = registry.getSubscriptionsForPif(currentKey);
										for (Subscription s : subscriptionsForPif) {
											registry.removeSubscription(s);
										}
										registry.removePrologInterface(currentKey);
										getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
									}

								}
							} catch (Throwable e) {
								Debug.report(e);
								return Status.CANCEL_STATUS;
							} finally {
								monitor.done();
							}
							return Status.OK_STATUS;
						}

					};
					j.schedule();
				} catch (Throwable t) {
					Debug.report(t);
				}
			}
		}



		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.STOP);
		}

		@Override
		public String getToolTipText() {
			return "Kill process";
		}

		@Override
		public String getText() {
			return "kill";
		}
	}


	private final class GenLoadFileAction extends Action {
		@Override
		public void run() {
			try {

				Job j = new UIJob("Generating load file") {

					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...",
									IProgressMonitor.UNKNOWN);

							if (getPrologInterface() != null) {
								List<String> consultedFiles = getPrologInterface().getConsultedFiles();
								
								// only create load file if there are consulted files
								if (consultedFiles != null && consultedFiles.size() > 0) {
									WizardDialog dialog = new WizardDialog(getViewSite().getShell(), new GenerateLoadFileWizard(consultedFiles));
									dialog.open();
								} else {
									MessageDialog.openWarning(PrologConsoleView.this.getViewSite().getShell(), "Generate Load File", "No need for a load file, since no files are consulted.");
								}
								
							}

						} catch (Throwable e) {
							Debug.report(e);
							return Status.CANCEL_STATUS;
						} finally {
							monitor.done();
						}
						return Status.OK_STATUS;
					}

				};
				j.schedule();
			} catch (Throwable t) {
				Debug.report(t);
			}

		}


		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.GEN_LOAD_FILE);
		}

		@Override
		public String getToolTipText() {
			return "Generate load file from consulted files";
		}

		@Override
		public String getText() {
			return "generateLoadFile";
		}
	}

	private final class CreateNamedProcessAction extends Action{

		@Override
		public void run(){
			Job j = new UIJob("Creating new Prolog Process")
			{
				@Override
				public IStatus runInUIThread(IProgressMonitor arg0) {
					PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();

					InputDialog dialog = createNewProcessNameDialog(registry);	
					int result = dialog.open();
					if (result == InputDialog.CANCEL)
						return Status.CANCEL_STATUS;
					String pifKey = dialog.getValue();

					PrologInterface pif = activateNewPrologProcess(registry, pifKey);
					pif.setAttribute(KILLABLE, "true");
					return Status.OK_STATUS;
				}

				public InputDialog createNewProcessNameDialog(
						PrologInterfaceRegistry registry) {
					final Set<String> pifKeys = registry.getRegisteredKeys();
					String defaultPifKey = getNameOfProjectOfActiveEditorInput();
					if (pifKeys.contains(defaultPifKey))
						defaultPifKey = null;

					IInputValidator validator = new IInputValidator(){
						@Override
						public String isValid(String arg0) {
							if ("".equals(arg0))
								return "Process name must not be empty";
							else if (pifKeys.contains(arg0))
								return "Process name already used";
							else
								return null;
						}
					};
					InputDialog dialog = new InputDialog(PrologConsoleView.this.getViewSite().getShell(), "Create Prolog Process", "Enter a new name for your new Prolog process:", defaultPifKey, validator);
					return dialog;
				}

			};
			j.schedule();
		}

		private String getNameOfProjectOfActiveEditorInput(){
			IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
			if (window != null) {
				IWorkbenchPage page = window.getActivePage();
				if (page != null) {
					IEditorPart editor = page.getActiveEditor();
					if (editor == null)
						return null;

					IEditorInput editorInput = editor.getEditorInput();
					if (editorInput == null)
						return null;

					IFile file = (IFile) editorInput.getAdapter(IFile.class);
					if (file != null) {
						return file.getProject().getName();
					}
				}
			}
			return null;
		}

		@Override
		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.NEW_PROCESS);
		}

		@Override
		public String getToolTipText() {
			return "create process";
		}

		@Override
		public String getText() {
			return "create process";
		}
	}

	private class ConsoleQueryAction extends Action {

		private String query;

		public ConsoleQueryAction(String text, ImageDescriptor icon, String query){
			super(text, icon);
			this.query = query.endsWith(".") ? query : query + ".";
			setToolTipText(text);
		}

		protected String getQuery(){
			return query;
		}

		@Override
		public void run(){
			Job j = new Job(getToolTipText()) {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					try {
						PrologConsole c = getConsole();
						ConsoleModel model = c.getModel();
						model.setLineBuffer(" ");
						model.commitLineBuffer();
						model.setLineBuffer(getQuery());
						model.commitLineBuffer();
					} catch (Throwable e) {
						Debug.report(e);
						return Status.CANCEL_STATUS;
					} finally {
						monitor.done();
					}
					return Status.OK_STATUS;
				}

				private PrologConsole getConsole() {
					return PrologConsoleView.this;
				}

			};
			j.schedule();
		}
	}

	private class PifQueryAction extends Action {

		private String query;

		public PifQueryAction(String text, ImageDescriptor icon, String query){
			super(text, icon);
			this.query = query.endsWith(".") ? query : query + ".";
			setToolTipText(text);
		}

		protected String getQuery(){
			return query;
		}

		@Override
		public void run(){
			try {
				getPrologInterface().queryOnce(getQuery());
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
		}
	}

	public static final String HOOK_ID = PDTConsole.CONSOLE_VIEW_ID;
	private ConsoleViewer viewer;
	private Composite partControl;
	private PrologInterface currentPif;
	private Menu contextMenu;
	private Action cutAction;
	private Action copyAction;
	private Action pasteAction;
	private Action selectAllAction;
	private ClearAction clearAction;
	//	private GuiTracerAction guiTracerAction;
	private PasteAction pasteFileNameAction;
	private RestartAction restartAction;
	private KillAction killAction;
	private GenLoadFileAction genLoadFileAction;
	private CreateNamedProcessAction createProcessAction;
	private HashMap<PrologInterface, PrologSocketConsoleModel> models = new HashMap<PrologInterface, PrologSocketConsoleModel>();
	private Label title;
	private HashMap<PrologInterface, SavedState> viewerStates = new HashMap<PrologInterface, SavedState>();

	private SelectContextPIFAutomatedAction automatedSelector;
	private ConsoleQueryAction activateGuiTracerAction;
	private ConsoleQueryAction deactivateGuiTracerAction;
	private ConsoleQueryAction threadMonitorAction;
	private ConsoleQueryAction debugMonitorAction;

	private PifQueryAction abortAction;
	private PifQueryAction traceAction;
	private Action helpAction;
	private Action configAction;

	@Override
	public void createPartControl(Composite parent) {

		try {
			createPartControl_impl(parent);
			PrologInterfaceRegistry registry = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
			if (registry.getAllKeys().size() == 0) {
				activateNewPrologProcess(registry, DEFAULT_CONSOLE);
			}
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t.getLocalizedMessage(), t);
		}
	}

	private void createPartControl_impl(Composite parent) {

		this.partControl = parent;

		Listener handler = new Listener() {

			@Override
			public void handleEvent(Event event) {
				switch (event.type) {
				case SWT.Show:
				case SWT.Hide:
					getDefaultPrologConsoleService().fireConsoleVisibilityChanged(PrologConsoleView.this);
					break;
				case SWT.FocusOut:
					getDefaultPrologConsoleService().fireConsoleLostFocus(PrologConsoleView.this);
				}

			}


		};
		parent.getParent().addListener(SWT.Show, handler);
		parent.getParent().addListener(SWT.Hide, handler);
		parent.getParent().addListener(SWT.FocusOut, handler);
		PrologConsolePlugin.getDefault().getPrologConsoleService().registerPrologConsole(this);
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().registerActivePrologInterfaceListener(this);
		GridLayout layout = new GridLayout(1, true);
		layout.horizontalSpacing = 0;
		layout.verticalSpacing = 0;
		layout.marginWidth = 0;
		layout.marginHeight = 0;
		parent.setLayout(layout);
		GridData ld = new GridData(GridData.FILL_HORIZONTAL);
		title = new Label(parent, SWT.HORIZONTAL);
		title.setLayoutData(ld);
		viewer = new ConsoleViewer(parent, SWT.BORDER | SWT.MULTI | SWT.WRAP
				| SWT.V_SCROLL);
		viewer.getControl().setEnabled(false);
		ld = new GridData(GridData.FILL_BOTH);
		viewer.getControl().setLayoutData(ld);
		createActions();
		initMenus(parent);
		initToolBars();
		getSite().setSelectionProvider(viewer);

	}

	private DefaultPrologConsoleService getDefaultPrologConsoleService() {
		return ((DefaultPrologConsoleService) PrologConsolePlugin.getDefault().getPrologConsoleService());
	}


	private void loadHistory(ConsoleHistory history) {

		try {
			FileInputStream in = new FileInputStream(getHistoryFile());
			history.loadHistory(in);
			in.close();
		} catch (IOException e) {
			Debug.report(e);
		}

	}

	private void createActions() {
		cutAction = new Action() {
			@Override
			public void run() {
				viewer.cut();
			}
		};

		copyAction = new Action() {
			@Override
			public void run() {
				viewer.copy();
			}
		};
		pasteAction = new Action() {
			@Override
			public void run() {
				viewer.paste();
			}
		};
		selectAllAction = new Action() {
			@Override
			public void run() {
				viewer.selectAll();
			}
		};
		clearAction = new ClearAction("Clear", "Clear console output",
				ImageRepository.getImageDescriptor(ImageRepository.CLEAR));
		//		guiTracerAction = new GuiTracerAction(new String[] {"guitracer", "noguitracer"},
		//				new String[] {"activate guitracer", "deactivate guitracer"},  
		//				new String[] {"activate GUI tracer", "deactivate GUI tracer"}, 
		//				new ImageDescriptor[] {
		//				ImageRepository.getImageDescriptor(ImageRepository.GUITRACER),
		//				ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER)});
		activateGuiTracerAction = new ConsoleQueryAction("Activate GUI tracer", ImageRepository.getImageDescriptor(ImageRepository.GUITRACER), "guitracer");
		deactivateGuiTracerAction = new ConsoleQueryAction("Deactivate GUI tracer", ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER), "noguitracer");
		threadMonitorAction = new ConsoleQueryAction("Show SWI thread monitor", ImageRepository.getImageDescriptor(ImageRepository.THREAD_MONITOR), "user:prolog_ide(thread_monitor)");
		debugMonitorAction = new ConsoleQueryAction("Show SWI debug message monitor", ImageRepository.getImageDescriptor(ImageRepository.DEBUG_MONITOR), "user:prolog_ide(debug_monitor)");
		abortAction = new PifQueryAction("Abort running query", ImageRepository.getImageDescriptor(ImageRepository.ABORT), "pdt_console_server:console_thread_name(ID), catch(thread_signal(ID, abort),_,fail)") {
			@Override
			public void run(){
				super.run();
				if (!getModel().isConnected()) {
					new Thread(new Runnable(){
						@Override
						public void run() {
							try {
								Thread.sleep(500);
							} catch (InterruptedException e) {
							}
							try {
								connect(currentPif);
							} catch (PrologInterfaceException e) {
							}
						}
					}).start();
				}
			}
		}; 		
		traceAction = new PifQueryAction(
				"Interrupt running query and start tracing",
				ImageRepository.getImageDescriptor(ImageRepository.TRACE),
				"pdt_console_server:console_thread_name(ID), catch(thread_signal(ID, trace),_,fail)");
		
		pasteFileNameAction = new PasteAction("Paste filename",
				"Paste the name of the current editor file", ImageRepository
				.getImageDescriptor(ImageRepository.PASTE_FILENAME)) {

			@Override
			protected String getTextToInsert() {
				String fileName = UIUtils.getFileFromActiveEditor();
				if (fileName == null) {
					return null;
				}
				return Util.quoteAtom(Util.prologFileName(new File(fileName)));
			}

		};
		pasteFileNameAction.setActionDefinitionId(PDTConsole.COMMAND_PASTE_FILENAME);

		IKeyBindingService keyBindingService = getSite().getKeyBindingService();
		keyBindingService.setScopes(new String[] { PDTConsole.CONTEXT_USING_CONSOLE_VIEW });
		keyBindingService.registerAction(pasteFileNameAction);
		restartAction = new RestartAction();
		killAction = new KillAction();
		genLoadFileAction = new GenLoadFileAction();
		createProcessAction = new CreateNamedProcessAction();
		helpAction = new Action("SWI-Prolog Documentation", ImageRepository.getImageDescriptor(ImageRepository.HELP)) {
			@Override
			public void run() {
				try {
					PlatformUI.getWorkbench().getBrowserSupport().getExternalBrowser().openURL(new URL("http://www.swi-prolog.org/pldoc/index.html"));
				} catch (Exception e) {}
			}

		};
		
		configAction = new Action("Console preferences", ImageRepository.getImageDescriptor(ImageRepository.PREFERENCES)) {
			@Override
			public void run() {
				PreferenceManager mgr = new PreferenceManager();
				
				IPreferencePage page = new PreferencePageMain();
				page.setTitle("PDT Console");
				
				IPreferenceNode node = new PreferenceNode("PreferencePage", page);
				mgr.addToRoot(node);

				PreferenceDialog dialog = new PreferenceDialog(getSite().getShell(), mgr);
				dialog.create();
				dialog.setMessage(page.getTitle());
				dialog.open();
			}
		};
	}

	private void initMenus(Control parent) {

		MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {

			@Override
			public void menuAboutToShow(IMenuManager manager) {
				manager.add(new Separator("#Clipboard"));

				IWorkbenchWindow window = getSite().getWorkbenchWindow();
				IWorkbenchAction sall = ActionFactory.SELECT_ALL.create(window);
				sall.setImageDescriptor(ImageRepository
						.getImageDescriptor(ImageRepository.SELECT_ALL));
				manager.add(sall);
				
				manager.add(ActionFactory.CUT.create(window));
				manager.add(ActionFactory.COPY.create(window));
				manager.add(ActionFactory.PASTE.create(window));
				manager.add(pasteFileNameAction);
				manager.add(clearAction);
				manager.add(new Separator("#Clipboard-end"));
				manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
				manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS + "-end"));
			}

		});
		getSite().registerContextMenu(manager, viewer);
		contextMenu = manager.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(contextMenu);
	}

	private void initToolBars() {
		IActionBars bars = this.getViewSite().getActionBars();

		bars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(),
				selectAllAction);
		bars.setGlobalActionHandler(ActionFactory.CUT.getId(), cutAction);
		bars.setGlobalActionHandler(ActionFactory.COPY.getId(), copyAction);
		bars.setGlobalActionHandler(ActionFactory.PASTE.getId(), pasteAction);

		IToolBarManager toolBarManager = bars.getToolBarManager();

		addToolbarContributions(toolBarManager);
		addMenuContributions(bars.getMenuManager());

		//		pifSelector.init(getViewSite().getWorkbenchWindow());
		automatedSelector.init(getViewSite().getWorkbenchWindow());

	}

	private void createAutomatedSelector(IToolBarManager toolBarManager) {

		automatedSelector = new SelectContextPIFAutomatedAction(){


			@Override
			protected PrologInterface getPrologInterface() {
				return PrologConsoleView.this.getPrologInterface();
			}


			@Override
			protected void setPrologInterface(PrologInterface prologInterface) {
				PrologConsoleView.this.setPrologInterface(prologInterface);

			}


			@Override
			protected void trackerActivated(PrologContextTracker tracker) {
				setPrologInterface(automatedSelector.getCurrentPrologInterface());

			}


			@Override
			protected void trackerDeactivated(PrologContextTracker tracker) {
				setPrologInterface(automatedSelector.getCurrentPrologInterface());

			}


			@Override
			public void contextChanged(PrologContextTrackerEvent e) {
				PrologContextTracker tracker = (PrologContextTracker) e
						.getSource();
				Debug.info("context changed for tracker " + tracker.getLabel());
				setPrologInterface(e.getPrologInterface());

			}

		};
		toolBarManager.add(automatedSelector);

		//		pifSelector = new SelectPifAction() {
		//
		//			protected void setPrologInterface(PrologInterface prologInterface) {
		//				PrologConsoleView.this.setPrologInterface(prologInterface);
		//
		//			}
		//
		//			protected PrologInterface getPrologInterface() {
		//				return PrologConsoleView.this.getPrologInterface();
		//			}
		//
		//		};
		//		toolBarManager.add(pifSelector);

		//		contextSelector = new SelectContextsAction() {
		//
		//			public void contextChanged(PrologContextTrackerEvent e) {
		//				PrologContextTracker tracker = (PrologContextTracker) e
		//						.getSource();
		//				Debug.info("context changed for tracker " + tracker.getLabel());
		//				setPrologInterface(e.getPrologInterface());
		//
		//			}
		//
		//			protected void trackerActivated(PrologContextTracker tracker) {
		//				setPrologInterface(contextSelector.getCurrentPrologInterface());
		//
		//			}
		//
		//			protected void trackerDeactivated(PrologContextTracker tracker) {
		//				setPrologInterface(contextSelector.getCurrentPrologInterface());
		//
		//			}
		//		};

		//		toolBarManager.add(contextSelector);
		//		setPrologInterface(contextSelector.getCurrentPrologInterface());
		setPrologInterface(automatedSelector.getCurrentPrologInterface());

	}

	public PrologInterface activateNewPrologProcess(PrologInterfaceRegistry registry, String pifKey) {
		DefaultSubscription subscription = new DefaultSubscription(pifKey + "_indepent", pifKey, "Independent prolog process", pifKey + " (Prolog)");
		registry.addSubscription(subscription);
		PrologInterface pif = PrologRuntimeUIPlugin.getDefault().getPrologInterface(subscription);

		if (automatedSelector.getActiveTrackers().isEmpty()){
			PrologConsoleView.this.setPrologInterface(pif);
			PrologConsoleView.this.automatedSelector.setImageDescriptor(ImageRepository.getImageDescriptor(ImageRepository.MANUAL_MODE));
		}
		return pif;
	}

	private void addToolbarContributions(IToolBarManager manager) {
		manager.add(new Separator("#Console"));
		createAutomatedSelector(manager);
		manager.add(createProcessAction);
		manager.add(restartAction);
		manager.add(killAction);
		manager.add(clearAction);
		manager.add(new Separator("#Console-end"));
		manager.add(new Separator("#Query"));
		manager.add(traceAction);
		manager.add(abortAction);
		manager.add(new Separator("#Query-end"));
		manager.add(new Separator("#Toolbar-Other"));
		manager.add(genLoadFileAction);
		manager.add(configAction);
		manager.add(helpAction);
		manager.add(new Separator("#Toolbar-End"));
	}

	private void addMenuContributions(IMenuManager manager) {
		manager.add(activateGuiTracerAction);
		manager.add(deactivateGuiTracerAction);
		manager.add(threadMonitorAction);
		manager.add(debugMonitorAction);
	}


	private File getHistoryFile() {
		String value = PrologConsolePlugin.getDefault().getPreferenceValue(
				PDTConsole.PREF_CONSOLE_HISTORY_FILE, null);
		if (value == null) {
			throw new NullPointerException("Required property \""
					+ PDTConsole.PREF_CONSOLE_HISTORY_FILE
					+ "\" was not specified.");
		}
		return new File(value);
	}

	@Override
	public void setFocus() {
		if (viewer == null) {
			Debug
			.warning("PrologConsoleView.setFocus(): View not instantiated yet.");
			return;
		}
		viewer.getControl().setFocus();
		getDefaultPrologConsoleService().fireConsoleRecievedFocus(this);
	}


	@Override
	public void dispose() {
		PrologConsolePlugin.getDefault().getPrologConsoleService()
		.unregisterPrologConsole(this);
		for (Iterator<PrologInterface> it = models.keySet().iterator(); it.hasNext();) {
			PrologInterface pif = it.next();
			try {
				disconnect(pif);
				removeHooks(pif);
			} catch (Throwable e) {
				Debug.report(e);
			}
		}
		models.clear();
		contextMenu.dispose();
		// viewer.getControl().dispose();
		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#onInit(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	public void onInit(PrologInterface pif, PrologSession initSession) {
		;
	}

	private void startServer(PrologInterface pif, PrologSession session) {
		try {
			String queryString = 
					"use_module(lib_pdt_console_pl(pdt_console_server)), "
							+ "pdt_console_server:pdt_start_console_server(Port, " + Util.quoteAtom(PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getKey(pif)) + ")";
			Debug.info("starting console server using: " + queryString);

			Map<String,?> result = session.queryOnce(queryString);
			if (result == null) {
				Debug.info("starting server failed, which may mean that it is actualy running already.");
				result = session.queryOnce("pdt_current_console_server(Port)");
				if(result==null){
					throw new RuntimeException("No Server running.");
				}
			}

			int port = Integer.parseInt((String) result.get("Port"));
			Debug.debug("A server thread seems to be listinging at port "+port);
		} catch (Throwable e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	@Override
	public void afterInit(PrologInterface pif) {
		try {
			connect(pif);
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	@Override
	public void beforeShutdown(PrologInterface pif, PrologSession session) {
		ConsoleHistory history = viewer.getHistory();
		saveHistory(history);
		disconnect(pif);
	}

	@Override
	public void onError(PrologInterface pif) {
		ConsoleHistory history = viewer.getHistory();
		saveHistory(history);
		disconnect(pif);


	}

	private void saveHistory(ConsoleHistory history) {
		if (history == null) {
			return;
		}
		try {
			FileOutputStream out = new FileOutputStream(getHistoryFile());
			history.saveHistory(out);
			out.close();
		} catch (IOException e) {
			Debug.report(e);
		}
	}

	@Override
	public ConsoleModel getModel() {
		return (getPrologInterface() == null ? null : models
				.get(getPrologInterface()));
	}

	@Override
	public PrologInterface getPrologInterface() {
		return currentPif;
	}

	@Override
	public void setPrologInterface(PrologInterface newPif) {
		if(currentPif==newPif){
			return;
		}
		if (currentPif != null) {
			viewerStates.put(currentPif, viewer.saveState());
		}
		currentPif = newPif;
		if (currentPif != null) {
			addHooks(currentPif);
			try {
				connect(currentPif);
			} catch (PrologInterfaceException e) {
				Debug.report(e);
			}
			reconfigureViewer(currentPif);
			getDefaultPrologConsoleService().fireActivePrologInterfaceChanged(this);
			PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService().setActivePrologInterface(currentPif);

		} else {
			Debug.debug("no pif (yet).");
		}
		if(automatedSelector != null){
			automatedSelector.update();
		}
	}
	
	@Override
	public void ensureConnectionForCurrentPrologInterface() {
		try {
			connect(currentPif);
		} catch (PrologInterfaceException e) {
			Debug.report(e);
		}
	}


	/*
	 * note: implementation should take into account, that this method might be
	 * called several times for the same pif, even during one single life cycle.
	 * 
	 * attach means: ensure a model exsists for this pif. ensure the model is
	 * connected. console only attach to a pif that is in UP state.
	 * 
	 */
	synchronized private void connect(final PrologInterface pif)
			throws PrologInterfaceException {

		PrologSocketConsoleModel model = getConsoleModel(pif);
		ensureConnection(pif, model);
	}

	private PrologSocketConsoleModel getConsoleModel(final PrologInterface pif) {
		PrologSocketConsoleModel model = models.get(pif);
		if (model == null) {
			model = new PrologSocketConsoleModel(false);
			models.put(pif, model);
		}
		return model;
	}

	private void ensureConnection(final PrologInterface pif,
			PrologSocketConsoleModel model) throws PrologInterfaceException {
		if (model.isConnected()) {
			return;
		}

		PrologSession session = pif.getSession(PrologInterface.NONE);
		FileSearchPathConfigurator.configureFileSearchPath(PrologRuntimeUIPlugin.getDefault()
				.getLibraryManager(), session,
				new String[] { PDTConsole.PL_LIBRARY });


		Map<String,?> result = null;
		try {
			result = session.queryOnce( "consult(lib_pdt_console_pl(loader)).");
			result = session.queryOnce( "pdt_start_console_server(Port, " + Util.quoteAtom(PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry().getKey(pif)) + ")");
			if (result == null) {
				startServer(pif, session);
				result = session.queryOnce("pdt_current_console_server(Port)");
			}
			if (result == null) {
				throw new RuntimeException("could not install console server");
			}
		} 
		catch (Exception e) {
			Debug.info(e.toString());
		}
		finally {
			if (session != null) {
				session.dispose();
			}
		}

		int port = Integer.parseInt(result.get("Port").toString());
		model.setPort(port);
		model.connect();
	}

	private void disconnect(PrologInterface pif) {
		PrologSocketConsoleModel model = models
				.get(pif);
		if (model == null) {
			return;
		}

		model.disconnect();

	}

	private void addHooks(PrologInterface pif) {
		pif.addLifeCycleHook(this, HOOK_ID, new String[0]);

	}

	private void removeHooks(PrologInterface pif) {

		pif.removeLifeCycleHook(HOOK_ID);

	}

	private void reconfigureViewer(final PrologInterface pif) {

		if (Display.getCurrent() != viewer.getControl().getDisplay()) {
			viewer.getControl().getDisplay().asyncExec(new Runnable() {
				@Override
				public void run() {
					reconfigureViewer(pif);
				}
			});
			return;
		}
		if (pif == null ) {

			viewer.setModel(null);
			viewer.setHistory(null);
			viewer.setCompletionProvider(null);
			title.setText("no console available(yet).");
			return;
		}

		ConsoleViewer.SavedState savedState = viewerStates
				.get(pif);
		if (savedState == null) {
			viewer.clearOutput();
			viewer.setModel(models.get(pif));
			PrologCompletionProvider completionProvider = new PrologCompletionProvider();
			completionProvider.setPrologInterface(pif);
			viewer.setCompletionProvider(completionProvider);
			ConsoleHistory history = new ConsoleHistory();
			viewer.setHistory(history);
			loadHistory(history);
		} else {
			viewer.loadState(savedState);
		}
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		String key = reg.getKey(pif);
		title.setText(key);

		viewer.setEnterSendsSemicolon(false);

	}


	@Override
	public boolean isVisible() {
		return partControl.getVisible();
	}

	public ConsoleViewer getViewer() {
		return viewer;
	}

	@Override
	public String getText() {
		return getViewer().getText();
	}

	@Override
	public int getLineAtOffset(int offset) {
		return getViewer().getLineAtOffset(offset);
	}

	@Override
	public int getOffsetAtLine(int line) {
		return getViewer().getOffsetAtLine(line);
	}

	@Override
	public int getLineCount() {
		return getViewer().getLineCount();
	}

	@Override
	public void clearOutput() {
		getViewer().clearOutput();

	}

	@Override
	public String getTextRange(int offset, int length) {
		return getViewer().getTextRange(offset, length);
	}

	@Override
	public int getCaretOffset() {
		return getViewer().getCaretOffset();
	}

	@Override
	public int getStartOfInput() {

		return getViewer().getStartOfInput();
	}

	@Override
	public void setCaretOffset(int offset) {
		getViewer().setCaretOffset(offset);

	}

	@Override
	public void setData(Object data) {
		;
	}

	@Override
	public void lateInit(PrologInterface pif) {
		;
	}

	@Override
	public void activePrologInterfaceChanged(PrologInterface pif) {
		setPrologInterface(pif);
	}

}