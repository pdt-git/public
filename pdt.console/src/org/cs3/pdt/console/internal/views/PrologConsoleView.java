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
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.console.internal.views.ConsoleViewer.SavedState;
import org.cs3.pdt.runtime.PLUtil;
import org.cs3.pdt.runtime.PrologContextTracker;
import org.cs3.pdt.runtime.PrologContextTrackerEvent;
import org.cs3.pdt.runtime.PrologInterfaceRegistry;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.NewConsoleHistory;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
import org.cs3.pl.metadata.MetaInfoProviderFactory;
import org.cs3.pl.prolog.LifeCycleHook;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IContributionManager;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.resource.ImageDescriptor;
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
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.actions.ActionFactory.IWorkbenchAction;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;

public class PrologConsoleView extends ViewPart implements LifeCycleHook,
		PrologConsole {
	private final class ClearAction extends Action {
		private ClearAction(String text, String tooltip, ImageDescriptor image) {
			super(text, image);
			setToolTipText(tooltip);
		}

		public void run() {
			getViewer().clearOutput();
		}
	}

	private final class ConsoleAction extends Action {
		private String query;

		public ConsoleAction(String query, String text, String tooltip,
				ImageDescriptor icon) {
			super(text, icon);
			this.query = query.trim().endsWith(".") ? query : query + ".";
			setToolTipText(tooltip);
		}

		public void run() {
			try {

				Job j = new Job(getToolTipText()) {

					protected IStatus run(IProgressMonitor monitor) {
						try {
							PrologConsole c = getConsole();
							ConsoleModel model = c.getModel();
							model.setLineBuffer(" ");
							model.commitLineBuffer();
							model.setLineBuffer(query);
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
			} catch (Throwable t) {
				Debug.report(t);
			}
		}
	}

	private final class RestartAction extends Action {
		public void run() {
			try {

				Job j = new UIJob("Restarting the PrologInterface") {

					public IStatus runInUIThread(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...",
									IProgressMonitor.UNKNOWN);

							try {
								if (currentPif != null) {
									currentPif.stop();
								}
								// setPrologInterface(getEditorPrologInterface());
							} finally {
								if (currentPif != null) {
									currentPif.start();
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

		public ImageDescriptor getImageDescriptor() {
			return ImageRepository.getImageDescriptor(ImageRepository.RESTART);
		}

		public String getToolTipText() {
			return "restart";
		}

		public String getText() {
			return "restart";
		}
	}

	public static final String HOOK_ID = "org.cs3.pdt.console.internal.views.PrologConsoleView";

	private ConsoleViewer viewer;

	// private PrologSocketConsoleModel model;

	private PrologCompletionProvider completionProvider;

	private Composite partControl;

	private Vector listeners = new Vector();

	private PrologInterface currentPif;

	private Menu contextMenu;

	private Action cutAction;

	private Action copyAction;

	private Action pasteAction;

	private Action selectAllAction;

	private ConsoleAction activateGuiTracerAction;

	private ClearAction clearAction;

	private ConsoleAction deactivateGuiTracerAction;

	private RestartAction restartAction;

	private NewConsoleHistory history;

	private SelectPifAction pifSelector;

	private HashMap models = new HashMap();

	private Label title;

	private SelectContextsAction contextSelector;

	private HashMap viewerStates=new HashMap();

	

	public void createPartControl(Composite parent) {

		try {
			createPartControl_impl(parent);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t.getLocalizedMessage(), t);
		}
	}

	private void createPartControl_impl(Composite parent) {

		this.partControl = parent;

		Listener handler = new Listener() {

			public void handleEvent(Event event) {
				switch (event.type) {
				case SWT.Show:
				case SWT.Hide:
					fireConsoleVisibilityChanged();
					break;
				case SWT.FocusOut:
					fireConsoleLostFocus();
				}

			}

		};
		parent.addListener(SWT.Show, handler);
		parent.addListener(SWT.Hide, handler);
		parent.addListener(SWT.FocusOut, handler);
		PrologConsolePlugin.getDefault().getPrologConsoleService()
				.registerPrologConsole(this);
		GridLayout layout = new GridLayout(1,true);
		layout.horizontalSpacing=0;
		layout.verticalSpacing=0;
		layout.marginWidth=0;
		layout.marginHeight=0;
		parent.setLayout(layout);
		GridData ld = new GridData(GridData.FILL_HORIZONTAL);
		title= new Label(parent,SWT.HORIZONTAL);
		title.setLayoutData(ld);
		viewer = new ConsoleViewer(parent, SWT.BORDER | SWT.MULTI | SWT.WRAP
				| SWT.V_SCROLL);
		viewer.getControl().setEnabled(false);
		ld=new GridData(GridData.FILL_BOTH);
		viewer.getControl().setLayoutData(ld);
		createActions();
		initMenus(parent);
		initToolBars();
		getSite().setSelectionProvider(viewer);
		
	}


	

	private void loadHistory() {

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
			public void run() {
				viewer.cut();
			}
		};

		copyAction = new Action() {
			public void run() {
				viewer.copy();
			}
		};
		pasteAction = new Action() {
			public void run() {
				viewer.paste();
			}
		};
		selectAllAction = new Action() {
			public void run() {
				viewer.selectAll();
			}
		};
		clearAction = new ClearAction("Clear", "clear console output",
				ImageRepository.getImageDescriptor(ImageRepository.CLEAR));
		activateGuiTracerAction = new ConsoleAction("guitracer",
				"activate guitracer", "activate GUI tracer", ImageRepository
						.getImageDescriptor(ImageRepository.GUITRACER));
		deactivateGuiTracerAction = new ConsoleAction("noguitracer",
				"deactivate guitracer", "deactivate GUI tracer",
				ImageRepository.getImageDescriptor(ImageRepository.NOGUITRACER));

		restartAction = new RestartAction();
	}

	private void initMenus(Control parent) {

		MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {

			public void menuAboutToShow(IMenuManager manager) {
				addContributions(manager);

			}

		});
		getSite().registerContextMenu(manager, viewer);
		contextMenu = manager.createContextMenu(parent);
		viewer.getControl().setMenu(contextMenu);
		// ContextMenuProvider menuProvider = new ContextMenuProvider();
		// menuProvider.addMenu(parent);

	}

	private void initToolBars() {
		IActionBars bars = this.getViewSite().getActionBars();

		bars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(),
				selectAllAction);
		bars.setGlobalActionHandler(ActionFactory.CUT.getId(), cutAction);
		bars.setGlobalActionHandler(ActionFactory.COPY.getId(), copyAction);
		bars.setGlobalActionHandler(ActionFactory.PASTE.getId(), pasteAction);
		IToolBarManager toolBarManager = bars.getToolBarManager();

		addContributions(toolBarManager);
		createCombo(toolBarManager);
		pifSelector.init(getViewSite().getWorkbenchWindow());
	}

	private void createCombo(IToolBarManager toolBarManager) {
		pifSelector = new SelectPifAction() {

			protected void setPrologInterface(PrologInterface prologInterface) {
				PrologConsoleView.this.setPrologInterface(prologInterface);

			}

			protected PrologInterface getPrologInterface() {
				return PrologConsoleView.this.getPrologInterface();
			}

		};
		toolBarManager.add(pifSelector);
		
		contextSelector = new SelectContextsAction(){

			public void contextChanged(PrologContextTrackerEvent e) {
				PrologContextTracker tracker = (PrologContextTracker) e.getSource();
				Debug.info("context changed for tracker "+tracker.getLabel());
				setPrologInterface(e.getPrologInterface());
				
			}

			protected void trackerActivated(PrologContextTracker tracker) {
				setPrologInterface(contextSelector.getCurrentPrologInterface());
				
			}

			protected void trackerDeactivated(PrologContextTracker tracker) {
				setPrologInterface(contextSelector.getCurrentPrologInterface());
				
			}
			
		};
		
		toolBarManager.add(contextSelector);
		setPrologInterface(contextSelector.getCurrentPrologInterface());
	}

	private void addContributions(IContributionManager manager) {
		IWorkbenchWindow window = getSite().getWorkbenchWindow();
		manager.add(new Separator("#ConsoleInternal"));
		manager.add(clearAction);
		manager.add(activateGuiTracerAction);
		manager.add(deactivateGuiTracerAction);
		manager.add(restartAction);
		manager.add(new Separator("#ConsoleInternal-end"));
		manager.add(new Separator("#Clipboard"));
		IWorkbenchAction sall = ActionFactory.SELECT_ALL.create(window);
		sall.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SELECT_ALL));
		manager.add(sall);
		manager.add(ActionFactory.COPY.create(window));
		manager.add(ActionFactory.CUT.create(window));
		manager.add(ActionFactory.PASTE.create(window));
		manager.add(new Separator("#Clipboard-end"));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS
				+ "-end"));
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

	public void setFocus() {
		if (viewer == null) {
			Debug
					.warning("PrologConsoleView.setFocus(): View not instantiated yet.");
			return;
		}
		viewer.getControl().setFocus();
		fireConsoleRecievedFocus();
	}

	private void fireConsoleRecievedFocus() {
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(this);
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleRecievedFocus(e);
		}
	}

	private void fireConsoleLostFocus() {
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(this);
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleLostFocus(e);
		}
	}

	private void fireConsoleVisibilityChanged() {
		Vector clone = null;
		synchronized (listeners) {
			clone = (Vector) listeners.clone();
		}
		PrologConsoleEvent e = new PrologConsoleEvent(this);
		for (Iterator iter = clone.iterator(); iter.hasNext();) {
			PrologConsoleListener l = (PrologConsoleListener) iter.next();
			l.consoleVisibilityChanged(e);
		}
	}

	public void dispose() {
		PrologConsolePlugin.getDefault().getPrologConsoleService()
				.unregisterPrologConsole(this);
		for (Iterator it = models.keySet().iterator(); it.hasNext();) {
			PrologInterface pif = (PrologInterface) it.next();
			try{
				detachFromPif(pif);
				removeHooks(pif);
			}catch (Throwable e) {
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
	public void onInit(PrologInterface pif, PrologSession initSession) {

	}

	private void startServer(PrologInterface pif, PrologSession session) {
		try {
			
			
			
			int port = Util.findFreePort();
			
			File lockFile = Util.getLockFile();
			String prologFileName = Util.prologFileName(lockFile);
			String queryString = "use_module(library(pdt_console_server)), " +
					"pdt_start_console_server("
				    + port
					+ ",'"
					+ prologFileName
					+ "')";
			Debug.info("starting console server using: " + queryString);
			Map map = session.queryOnce(queryString);
			if(map==null){
				Debug.info("starting server failed, which may mean that it is actualy running already.");
				map = session.queryOnce("pdt_current_console_server(Port,LockFile)");				
				lockFile=new File((String) map.get("LockFile"));
				Debug.info("Yep. got this lockfile: "+lockFile.getCanonicalPath());
			}
			

			while (!lockFile.exists()) {
				try {
					Thread.sleep(50);
				} catch (InterruptedException e1) {
					Debug.report(e1);
				}
			}
			Debug.debug("Server thread created");

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
	public void afterInit(PrologInterface pif) {
		// viewer.setController(controller);
		attachToPif(pif);
//		if(pif==currentPif){
//			reconfigureViewer(pif);
//		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologInterface pif, PrologSession session) {
		saveHistory();
		detachFromPif(pif);
		if(pif==currentPif){
			viewerStates.put(pif,viewer.saveState());
		}
	}

	private void saveHistory() {
		try {
			FileOutputStream out = new FileOutputStream(getHistoryFile());
			history.saveHistory(out);
			out.close();
		} catch (IOException e) {
			Debug.report(e);
		}
	}

	public ConsoleModel getModel() {
		return (ConsoleModel) (currentPif==null?null:models.get(currentPif));
	}

	public PrologInterface getPrologInterface() {
		return currentPif;
	}

	public void setPrologInterface(PrologInterface newPif) {
		if (currentPif != null) {
			viewerStates.put(currentPif,viewer.saveState());
		}
		this.currentPif = newPif;
		if (currentPif != null) {
			addHooks(currentPif);
			attachToPif(currentPif);
			reconfigureViewer(currentPif);

		} else {
			Debug.debug("no pif (yet).");
		}
		if (pifSelector != null) {
			pifSelector.update();
		}
	}

	/*
	 * note: implementation should take into account, that this method might
	 * be called several times for the same pif, even during one single life cycle.
	 */
	private void attachToPif(PrologInterface pif) {
		
		PrologSocketConsoleModel model = (PrologSocketConsoleModel) models
				.get(pif);
		if (model == null) {
			model = new PrologSocketConsoleModel(false);
			models.put(pif, model);
		}
		if (model.isConnected()) {
			return;
		}
		PrologSession session = pif.getSession();
		PLUtil.configureFileSearchPath(PrologRuntimePlugin.getDefault().getLibraryManager(),session,new String[]{PDTConsole.PL_LIBRARY});
		Map m = null;
		try {
			m = session
					.queryOnce("use_module(library(pdt_console_server))," +
							"pdt_current_console_server(Port,LockFile)");

			if (m == null) {
				startServer(pif, session);
				m = session.queryOnce("pdt_current_console_server(Port,LockFile)");
;
			}
			if (m == null) {
				// now we really have a problem
				throw new RuntimeException("could not install console server");
			}
		} finally {
			if (session != null) {
				session.dispose();
			}
		}
		int port = Integer.parseInt(m.get("Port").toString());
		File lockFile = new File((String) m.get("LockFile"));

		model.setPort(port);
		model.setLockFile(lockFile);
		model.connect();

	}

	private void detachFromPif(PrologInterface pif) {
		PrologSocketConsoleModel model = (PrologSocketConsoleModel) models
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
				public void run() {
					reconfigureViewer(pif);
				}
			});
			return;
		}
		if (pif == null || !pif.isUp()) {
			viewer.getControl().setEnabled(false);
			viewer.setModel(null);
			viewer.setHistory(null);
			viewer.setCompletionProvider(null);
			title.setText("no console available(yet).");
			return;
		}

		ConsoleViewer.SavedState savedState = (SavedState) viewerStates.get(pif);
		if(savedState==null){
			viewer.clearOutput();
			viewer.setModel((ConsoleModel) models.get(pif));
			completionProvider = new PrologCompletionProvider();
			completionProvider
					.setPrologInterface(pif);
			viewer.setCompletionProvider(completionProvider);
			history = new NewConsoleHistory();
			viewer.setHistory(history);
			loadHistory();
		}
		else{
			viewer.loadState(savedState);
		}
		PrologInterfaceRegistry reg = PrologRuntimePlugin.getDefault().getPrologInterfaceRegistry();
		String key = reg.getKey(pif);
		String label = reg.getName(key);
		label=label==null?key:label;
		title.setText(label);
		boolean useEnter = Boolean.valueOf(
				PrologConsolePlugin.getDefault().getPreferenceValue(
						PDTConsole.PREF_ENTER_FOR_BACKTRACKING, "false"))
				.booleanValue();
				
		viewer.setEnterSendsSemicolon(useEnter);
		viewer.getControl().setEnabled(true);
	}

	public void addPrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (!listeners.contains(l)) {
				listeners.add(l);
			}
		}

	}

	public void removePrologConsoleListener(PrologConsoleListener l) {
		synchronized (listeners) {
			if (listeners.contains(l)) {
				listeners.remove(l);
			}
		}
	}

	public boolean isVisible() {
		return partControl.isVisible();
	}

	public ConsoleViewer getViewer() {
		return viewer;
	}

	public String getText() {
		return getViewer().getText();
	}

	public int getLineAtOffset(int offset) {
		return getViewer().getLineAtOffset(offset);
	}

	public int getOffsetAtLine(int line) {
		return getViewer().getOffsetAtLine(line);
	}

	public int getLineCount() {
		return getViewer().getLineCount();
	}

	public void clearOutput() {
		getViewer().clearOutput();

	}

	public String getTextRange(int offset, int length) {
		return getViewer().getTextRange(offset, length);
	}

	public int getCaretOffset() {
		return getViewer().getCaretOffset();
	}
}