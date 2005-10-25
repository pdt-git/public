package org.cs3.pdt.console.internal.views;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Iterator;
import java.util.Map;
import java.util.Vector;

import org.cs3.pdt.console.PDTConsole;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.console.internal.ImageRepository;
import org.cs3.pdt.console.internal.hooks.ConsoleServerHook;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.console.ConsoleModel;
import org.cs3.pl.console.DefaultConsoleHistory;
import org.cs3.pl.console.prolog.PrologConsole;
import org.cs3.pl.console.prolog.PrologConsoleEvent;
import org.cs3.pl.console.prolog.PrologConsoleListener;
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
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.part.ViewPart;

public class PrologConsoleView extends ViewPart implements LifeCycleHook,
		PrologConsole {
	private final class ClearAction extends Action {
		private ClearAction(String text, String tooltip,ImageDescriptor image) {
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
			this.query = query.trim().endsWith(".")?query:query+".";
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

				Job j = new Job("Restarting the PrologInterface") {

					protected IStatus run(IProgressMonitor monitor) {
						try {
							monitor.beginTask("initializing...",
									IProgressMonitor.UNKNOWN);

							try {
								pif.stop();
							} finally {
								pif.start();
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

	private PrologSocketConsoleModel model;

	private PrologCompletionProvider completionProvider;

	private Composite partControl;

	private Vector listeners = new Vector();

	private PrologInterface pif;

	private Menu contextMenu;

	private Action cutAction;

	private Action copyAction;

	private Action pasteAction;

	private Action selectAllAction;

	private ConsoleAction activateGuiTracerAction;

	private ClearAction clearAction;

	private ConsoleAction deactivateGuiTracerAction;

	private RestartAction restartAction;

	private DefaultConsoleHistory history;

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
		this.pif = PrologRuntimePlugin.getDefault().getPrologInterface();
		PrologConsolePlugin.getDefault().getPrologConsoleService()
				.registerPrologConsole(this);
		viewer = new ConsoleViewer(parent, SWT.BORDER | SWT.MULTI | SWT.WRAP
				| SWT.V_SCROLL);

		pif.addLifeCycleHook(this, HOOK_ID,
				new String[] { ConsoleServerHook.HOOK_ID });

		completionProvider = new PrologCompletionProvider();
		completionProvider.setMetaInfoProvider(PDTCorePlugin.getDefault()
				.getMetaInfoProvider());
		viewer.setCompletionProvider(completionProvider);
		
		history = new DefaultConsoleHistory();
		
		viewer.setHistory(history);
		
		
		configureAndConnect();
		createActions();
		initMenus(parent);
		getSite().setSelectionProvider(viewer);
		loadHistory();
		
		

	}

	private void configureAndConnect() {
		PrologSession session = pif.getSession();
		
		Map m = null;
		try{
			 m = session.queryOnce("pdt_console_server(Port,LockFile)");
		}finally{
			if(session!=null){
				session.dispose();
			}
		}
		int port = Integer.parseInt(m.get("Port").toString());
		File lockFile = new File((String) m.get("LockFile"));
		if(model==null){
			model = new PrologSocketConsoleModel(false);
		}
		model.setPort(port);
		model.setLockFile(lockFile);
		
		viewer.setModel(model);
		if (lockFile.exists()) {
			model.connect();
		}
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
		// ActionFactory.IWorkbenchAction copyAction = ActionFactory.COPY
		// .create(getViewSite().getWorkbenchWindow());
		// ActionFactory.IWorkbenchAction selectAllAction =
		// ActionFactory.SELECT_ALL
		// .create(getViewSite().getWorkbenchWindow());
		// ActionFactory.IWorkbenchAction cutAction = ActionFactory.CUT
		// .create(getViewSite().getWorkbenchWindow());
		// ActionFactory.IWorkbenchAction pasteAction = ActionFactory.PASTE
		// .create(getViewSite().getWorkbenchWindow());
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
		clearAction = new ClearAction("Clear","clear console output", ImageRepository
						.getImageDescriptor(ImageRepository.CLEAR));
		activateGuiTracerAction = new ConsoleAction("guitracer", "activate guitracer",
				"activate GUI tracer", ImageRepository
						.getImageDescriptor(ImageRepository.GUITRACER));
		deactivateGuiTracerAction = new ConsoleAction("noguitracer", "deactivate guitracer",
				"deactivate GUI tracer", ImageRepository
						.getImageDescriptor(ImageRepository.NOGUITRACER));

		restartAction = new RestartAction();
	}

	private void initMenus(Control parent) {
		IToolBarManager toolbarMenu = getViewSite().getActionBars()
				.getToolBarManager();

		MenuManager manager = new MenuManager();
		manager.setRemoveAllWhenShown(true);
		manager.addMenuListener(new IMenuListener() {

			public void menuAboutToShow(IMenuManager manager) {
				addContributions(manager);

			}

		});
		
		IActionBars bars = this.getViewSite().getActionBars();
		bars.setGlobalActionHandler(ActionFactory.SELECT_ALL.getId(),
				selectAllAction);
		bars.setGlobalActionHandler(ActionFactory.CUT.getId(), cutAction);
		bars.setGlobalActionHandler(ActionFactory.COPY.getId(), copyAction);
		bars.setGlobalActionHandler(ActionFactory.PASTE.getId(), pasteAction);
		addContributions(toolbarMenu);
		getSite().registerContextMenu(manager, viewer);
		contextMenu = manager.createContextMenu(parent);
		viewer.getControl().setMenu(contextMenu);
		// ContextMenuProvider menuProvider = new ContextMenuProvider();
		// menuProvider.addMenu(parent);
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
		manager.add(ActionFactory.SELECT_ALL.create(window));
		manager.add(ActionFactory.COPY.create(window));
		manager.add(ActionFactory.CUT.create(window));
		manager.add(ActionFactory.PASTE.create(window));
		manager.add(new Separator("#Clipboard-end"));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS
				+ "-end"));
	}

	private File getHistoryFile(){
		String value = PrologConsolePlugin.getDefault().getPreferenceValue(
				PDTConsole.PREF_CONSOLE_HISTORY_FILE, null);
		if (value == null) {
			throw new NullPointerException("Required property \""
					+ PDTConsole.PREF_CONSOLE_HISTORY_FILE + "\" was not specified.");
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#afterInit()
	 */
	public void afterInit(PrologInterface pif) {
		// viewer.setController(controller);
		configureAndConnect();
		
		loadHistory();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.cs3.pl.prolog.LifeCycleHook#beforeShutdown(org.cs3.pl.prolog.PrologSession)
	 */
	public void beforeShutdown(PrologInterface pif, PrologSession session) {
		// viewer.setController(null);
		model.disconnect();
		saveHistory();
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
		return model;
	}

	public PrologInterface getPrologInterface() {
		return pif;
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