package org.cs3.jlmp.internal.views;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.internal.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
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

	private DrillDownAdapter drillDownAdapter;

	private Action action1;

	private Action action2;

	private Action doubleClickAction;

	private ViewContentProvider contentProvider;

	/**
	 * @throws CoreException
	 */
	static PrologSession getPrologSession() throws CoreException {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		JLMPProjectNature nature = null;
		for (int i = 0; i < projects.length; i++) {
			if(projects[i].hasNature(JLMP.NATURE_ID)){
				nature = (JLMPProjectNature)projects[i].getNature(JLMP.NATURE_ID);
				break;
			}
		}
		if(nature == null)
			return null;
		return nature.getPrologInterface().getSession();
	}
	
	class ViewContentProvider implements IStructuredContentProvider,
			ITreeContentProvider {
		public IPEFNode[] roots;

		public void inputChanged(Viewer v, Object oldInput, Object newInput) {
			//System.out.println("DEBUG");
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
			PrologSession session = null; 
			try {
				session = getPrologSession();
//					    		getActiveFile().getProject().getNature(JLMP.NATURE_ID)).getPrologInterface();
//
				roots = new IPEFNode[1];
				roots[0] = PEFNode.find(getViewSite(), null, "100001");
				if(roots[0] == null)
					roots = new IPEFNode[0];
	        } catch (CoreException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			} finally {
	        	if(session != null) {
	        		session.dispose();
	        	}
	        }
		}



		public Object getParent(Object child) {
			if (child instanceof IPEFNode) {
				return ((IPEFNode) child).getParent();
			}
			return null;
		}

		public Object[] getChildren(Object parent) {
			if (parent instanceof IPEFNode) {
				return ((IPEFNode) parent).getChildren().toArray(new Object[0]);
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
		viewer = new TreeViewer(parent, SWT.MULTI | SWT.H_SCROLL | SWT.V_SCROLL);
		drillDownAdapter = new DrillDownAdapter(viewer);
		contentProvider = new ViewContentProvider();
		viewer.setContentProvider(contentProvider
		//				new WorkbenchContentProvider(){
				//		    /* (non-Javadoc)
				//             * @see
				// org.eclipse.ui.model.BaseWorkbenchContentProvider#hasChildren(java.lang.Object)
				//             */
				//            public boolean hasChildren(Object element) {
				//                if(element instanceof INode){
				//                    try{
				//                        INode node = (INode) element;
				//                        return node.hasChildren();
				//                    }
				//                    catch(Throwable t){
				//                        ;
				//                    }
				//                }
				//                return super.hasChildren(element);
				//            }
				//		}
				);

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
	}

	private void fillContextMenu(IMenuManager manager) {
		manager.add(action1);
		drillDownAdapter.addNavigationActions(manager);
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(action1);
		drillDownAdapter.addNavigationActions(manager);
	}

	private void makeActions() {
		action1 = new Action() {
			public void run() {
				//showMessage("Action 1 executed");
				List list = new ArrayList();
				for (int i = 0; i < contentProvider.roots.length; i++) {
					list.add(contentProvider.roots[i]);
				}
				InputDialog dialog = new InputDialog(PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell(),
						"JTransformer", "id", null, null);
				if (dialog.open() != Window.CANCEL) {
					final String value = dialog.getValue();
					IPEFNode node;
					node = PEFNode.find(getViewSite(),  null, value);
					if (node != null) {
						list.add(node);
						contentProvider.roots = (IPEFNode[]) list
								.toArray(new IPEFNode[0]);
						viewer.setInput(getViewSite());
					}
				}
			}
		};
		action1.setText("by Id");
		action1.setToolTipText("Add Element by Id");
		action1.setImageDescriptor(PlatformUI.getWorkbench().getSharedImages()
				.getImageDescriptor(ISharedImages.IMG_OBJS_INFO_TSK));
		doubleClickAction = new Action() {
			public void run() {
				ISelection selection = viewer.getSelection();
				if (!selection.isEmpty()) {
					PEFNode node = (PEFNode) ((IStructuredSelection) selection)
							.getFirstElement();
					if (node.getErrors().size() > 0)
						MessageDialog.openInformation(getViewSite().getShell(),
								"JTransformer", "Errors in node: \n"
										+ node.getErrorMessages());
					else {
						if(!node.isList()) {
							PrologSession session = null;
							try {
								session = getPrologSession();
								Map ht = session.queryOnce("gen_tree("+ node.getId() + ",Src)");
								if(ht != null && !ht.isEmpty())
									MessageDialog.openInformation(getViewSite().getShell(),
											"JTransformer - Src for id "+node.getId(), 
											(String)ht.get("Src"));						
							} catch (CoreException e) {
								// TODO Auto-generated catch block
								e.printStackTrace();
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

	private void showMessage(String message) {
		MessageDialog.openInformation(viewer.getControl().getShell(),
				"Prolog Navigator", message);
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
}