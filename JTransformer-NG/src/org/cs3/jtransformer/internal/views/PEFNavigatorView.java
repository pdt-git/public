package org.cs3.jtransformer.internal.views;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.JTransformerPlugin;
import org.cs3.pdt.console.PrologConsolePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.DoubleClickEvent;
import org.eclipse.jface.viewers.IDoubleClickListener;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerSorter;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.model.WorkbenchLabelProvider;
import org.eclipse.ui.part.DrillDownAdapter;
import org.eclipse.ui.part.ViewPart;


public class PEFNavigatorView extends ViewPart {
	private TreeViewer viewer;
	public static final String ID = "org.cs3.jtransformer.internal.navigator.PEFNavigatorView";

	private DrillDownAdapter drillDownAdapter;

	private Action action1;

	private Action action2;

	private Action doubleClickAction;

	private ViewContentProvider contentProvider;
	private Action actionClear;
	
	static private PEFNavigatorView pefNavigatorInstance;

	static private PrologInterface getPrologInterface()
	{
		try {
			return PrologConsolePlugin.getDefault().getPrologConsoleService().getActivePrologConsole().getPrologInterface();
		} catch(Exception ex) {
			throw new IllegalStateException("Could not find an active prolog console.");
		}
	}

	class ViewContentProvider implements IStructuredContentProvider,
			ITreeContentProvider {
		public IPEFNode[] roots;

		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
		}

		public void dispose() {
		}

		public Object[] getElements(Object parent) {
			if (parent.equals(getViewSite())) {
				if (roots == null)
					try {
						initializeRoots();
					} catch (IOException e) {
						e.printStackTrace();
					}
				return roots;
			}
			return getChildren(parent);
		}

		private void initializeRoots() throws IOException {
			roots = new IPEFNode[0];
		}



		public Object getParent(Object child) {
			if (child instanceof IPEFNode) {
				return ((IPEFNode) child).getParent();
			}
			return null;
		}

		public Object[] getChildren(Object parent) {
			if (parent instanceof IPEFNode) {
				try
				{
					return ((IPEFNode) parent).getChildren().toArray(new Object[0]);
				} catch (PrologInterfaceException e)
				{
					UIUtils.logAndDisplayError(
							JTransformerPlugin.getDefault().getErrorMessageProvider(),
							UIUtils.getDisplay().getActiveShell(),
							JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
							JTransformer.ERR_CONTEXT_ACTION_FAILED,
							e
							);
				}
			}
			return new Object[0];
		}

		public boolean hasChildren(Object parent) {
			if (parent instanceof IPEFNode)
				return ((IPEFNode) parent).hasChildren();
			return false;
		}
		/*
		 * We will set up a dummy model to initialize tree heararchy. In a real
		 * code, you will connect to a real model and expose its hierarchy.
		 */

	}

	class NameSorter extends ViewerSorter {
	}

	/**
	 * The constructor.
	 */
	public PEFNavigatorView() {
	}

	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	public void createPartControl_impl(Composite parent) {
		pefNavigatorInstance = this;
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		drillDownAdapter = new DrillDownAdapter(viewer);
		contentProvider = new ViewContentProvider();
		viewer.setContentProvider(contentProvider);

		viewer.setLabelProvider(new WorkbenchLabelProvider());
		viewer.setSorter(new NameSorter());
		viewer.setInput(getViewSite());
		getSite().setSelectionProvider(viewer);
		makeActions();
		hookContextMenu();
		hookDoubleClickAction();
		contributeToActionBars();
	}

	private void hookContextMenu() {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				PEFNavigatorView.this.fillContextMenu(manager);
			}
		});
		Menu menu = menuMgr.createContextMenu(viewer.getControl());
		viewer.getControl().setMenu(menu);
		getSite().registerContextMenu(menuMgr, viewer);
	}

	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(action1);
		manager.add(action2);
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(action1);
		manager.add(action2);
		manager.add(actionClear);
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
		manager.add(action2);
		manager.add(actionClear);
		
		drillDownAdapter.addNavigationActions(manager);
	}

	private void addNewNodeById(String id,PrologInterface pef) throws PrologInterfaceException {
		IPEFNode node = PEFNode.find(getViewSite(), null, id, null,pef);
		if (node != null) {
			List list = new ArrayList();
			for (int i = 0; i < contentProvider.roots.length; i++) {
				list.add(contentProvider.roots[i]);
			}
			list.add(node);
			contentProvider.roots = (IPEFNode[]) list
					.toArray(new IPEFNode[0]);
			viewer.setInput(getViewSite());
		}
	}

	private void makeActions() {
		action1 = new Action() {
			public void run() {
				InputDialog dialog = new InputDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
						"JTransformer", "id", null, null);
				if (dialog.open() != Window.CANCEL) {
					final String value = dialog.getValue();
					try
					{
						addNewNodeById(value,getPrologInterface());
					} catch (PrologInterfaceException e)
					{
						UIUtils.logAndDisplayError(
								JTransformerPlugin.getDefault().getErrorMessageProvider(),
								UIUtils.getDisplay().getActiveShell(),
								JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
								JTransformer.ERR_CONTEXT_ACTION_FAILED,
								e
								);
					}
				}
			}
		};
		action1.setText("by Id");
		action1.setToolTipText("Add Element by Id");
		action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
				.getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));

		action2 = new Action() {
			public void run() {
				//showMessage("Action 1 executed");
				InputDialog dialog = new InputDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
						"JTransformer", "full qualified name", null, null);
				if (dialog.open() != Window.CANCEL) {
					final String value = dialog.getValue();
					PrologSession session = null;

					try {
						PrologInterface pef = getPrologInterface();
						session = pef.getSession();
						Map result = session.queryOnce("fullQualifiedName(Id,'" + value+"')");
						if(result != null) {
							String id = (String)result.get("Id");
							if(id == null)
								MessageDialog.openError(getViewSite().getShell(),
										"JTransformer", "No class found with full qualified name: "+ value);
							addNewNodeById(id,pef);
						}
					} catch (IllegalStateException e) {
						MessageDialog.openError(getViewSite().getShell(),
								"JTransformer", 
								"Could not find an active prolog console. Please open the console view and select.");
					} catch (PrologInterfaceException e)
					{
						UIUtils.logAndDisplayError(
								JTransformerPlugin.getDefault().getErrorMessageProvider(),
								UIUtils.getDisplay().getActiveShell(),
								JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
								JTransformer.ERR_CONTEXT_ACTION_FAILED,
								e
								);
					} finally  {
						if (session != null)
						session.dispose();
					}
				}
			}
		};
		action2.setText("by full qualified name");
		action2.setToolTipText("Add class by full qualified name");
		action2.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
				.getImageDescriptor(ISharedImages.IMG_TOOL_NEW_WIZARD));

		actionClear = new Action() {
			public void run() {
				contentProvider.roots = new IPEFNode[0];
				viewer.setInput(getViewSite());
			}
		};
		actionClear.setText("clear");
		actionClear.setToolTipText("clear the PEF navigator");
		actionClear.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
				.getImageDescriptor(ISharedImages.IMG_TOOL_CUT));

		
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				if (!selection.isEmpty()) {
					PEFNode node = (PEFNode)((IStructuredSelection) selection)
							.getFirstElement();
					if (node.getErrors().size() > 0)
						MessageDialog.openInformation(getViewSite().getShell(),
								"JTransformer", "Errors in node: \n"
										+ node.getErrorMessages());
					else {
						if(!node.isList()) {
							PrologSession session = null;
							try {
								session = node.getPrologInterface().getSession();
								Map ht = session.queryOnce("gen_tree("+ node.getId() + ",Src)");
								if(ht != null && !ht.isEmpty())
									MessageDialog.openInformation(getViewSite().getShell(),
											"JTransformer - Src for id "+node.getId(), 
											(String)ht.get("Src"));						
							} catch (PrologInterfaceException e)
							{
								UIUtils.logAndDisplayError(
										JTransformerPlugin.getDefault().getErrorMessageProvider(),
										UIUtils.getDisplay().getActiveShell(),
										JTransformer.ERR_PROLOG_INTERFACE_EXCEPTION,
										JTransformer.ERR_CONTEXT_ACTION_FAILED,
										e
										);
							}finally{
								if(session != null)
									session.dispose();
							}
						}
					}
				}
			}
		};
	}

	private void hookDoubleClickAction() {
		viewer.addDoubleClickListener(new IDoubleClickListener() {
			public void doubleClick(DoubleClickEvent event) {
				doubleClickAction.run();
			}
		});
	}


	/**
	 * Passing the focus request to the viewer's control.
	 */
	public void setFocus_impl() {
		viewer.getControl().setFocus();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	public void createPartControl(Composite parent) {
		try {
			createPartControl_impl(parent);
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	public void setFocus() {
		try {
			setFocus_impl();
		} catch (Throwable t) {
			Debug.report(t);
			throw new RuntimeException(t);
		}
	}

	public static void addId(String value, PrologInterface prologInterface) throws PrologInterfaceException{
		pefNavigatorInstance.addNewNodeById(value,prologInterface);
	}
}