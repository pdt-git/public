package pdt.pefgraph.internal.views;

import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.cs3.pdt.core.PEFHandle;
import org.cs3.pl.common.Debug;
import org.cs3.pl.common.Util;
import org.cs3.pl.prolog.PrologInterface;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuListener;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.mylyn.zest.core.viewers.GraphViewer;
import org.eclipse.mylyn.zest.core.widgets.ZestStyles;
import org.eclipse.mylyn.zest.layouts.LayoutStyles;
import org.eclipse.mylyn.zest.layouts.algorithms.RadialLayoutAlgorithm;
import org.eclipse.mylyn.zest.layouts.algorithms.SpringLayoutAlgorithm;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.part.ViewPart;

import pdt.pefgraph.internal.ImageRepository;

public class PEFGraphView extends ViewPart{

	private final class QueryAction extends Action {
		private final PrologInterface pif;
		private final String action;

		private QueryAction(String text, int style, PrologInterface pif,
				String action) {
			super(text, style);
			this.pif = pif;
			this.action = action;
		}

		@Override
		public void run() {
			PrologSession s=null;
			try{
				s=pif.getSession();
				s.queryOnce("pef_graph_action_run("+action+")");
			}catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			}finally{
				if(s!=null){
					s.dispose();
				}
			}
		}
	}








	private PEFGraphContentProvider contentProvider;
	
	private Action selectPifAction;

	private Action refreshAction;

	private Action clearAction;

	private Action setVisibilityAction;

	private Menu contextMenu;

	private GraphViewer viewer;

	private PEFGraphLabelProvider labelProvider;


	/**
	 * This is a callback that will allow us to create the viewer and initialize
	 * it.
	 */
	public void createPartControl(Composite parent) {
		this.contentProvider=new PEFGraphContentProvider();
		this.labelProvider=new PEFGraphLabelProvider();
		this.viewer = new GraphViewer(parent,SWT.NONE);
		viewer.setLabelProvider(labelProvider);
		viewer.setContentProvider(contentProvider);
		viewer.setLayoutAlgorithm(new SpringLayoutAlgorithm(LayoutStyles.NO_LAYOUT_NODE_RESIZING));
		viewer.setConnectionStyle(ZestStyles.CONNECTIONS_DIRECTED);
		getSite().setSelectionProvider(viewer);
		//viewer.setLayoutAlgorithm(new RadialLayoutAlgorithm(LayoutStyles.NO_LAYOUT_NODE_RESIZING));
		makeActions();
		hookContextMenu(parent);
		contributeToActionBars();
		
		// addSelectionChangedListener(new ISelectionChangedListener(){
		//
		// public void selectionChanged(SelectionChangedEvent event) {
		// ISelection s = event.getSelection();
		// IStructuredSelection ss = null;
		// if(!(s instanceof IStructuredSelection)){
		// return;
		// }
		// ss=(IStructuredSelection) s;
		// Object[] array = ss.toArray();
		// for (int i = 0; i < array.length; i++) {
		// if(array[i] instanceof Node){
		// Node node = (Node)array[i];
		// makeInteresting(node);
		// }
		// }
		//				
		// }
		//			
		// });

	}

	
	
	private void hookContextMenu(Composite parent) {
		MenuManager menuMgr = new MenuManager("#PopupMenu");
		menuMgr.setRemoveAllWhenShown(true);
		menuMgr.addMenuListener(new IMenuListener() {
			public void menuAboutToShow(IMenuManager manager) {
				PEFGraphView.this.fillContextMenu(manager);
			}
		});
		getSite().registerContextMenu(menuMgr, viewer);
		contextMenu = menuMgr.createContextMenu(parent);
		viewer.getControl().setMenu(contextMenu);
	}
	
		
	
	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalPullDown(IMenuManager manager) {
		manager.add(clearAction);
		manager.add(setVisibilityAction);
		manager.add(selectPifAction);
		manager.add(refreshAction);
	}

	private void fillContextMenu(IMenuManager manager) {
		
		manager.add(clearAction);
		manager.add(setVisibilityAction);
		manager.add(selectPifAction);
		manager.add(refreshAction);
		
		PrologInterface pif = (PrologInterface) viewer.getInput();
		if(pif!=null&&pif.isUp()){
			manager.add(new Separator());
			
			String selectionList = "["+Util.splice(getSelectedIds(), ",")+"]";
			PrologSession s = null;
			try{
				s=pif.getSession();
				List l = s.queryAll("pef_graph_action("+selectionList+",Action,Path,Label,Style)");
				for (Object object : l) {
					Map m = (Map) object;
					String action = (String) m.get("Action");
					String path = Util.splice((List) m.get("Path"),"/");
					String label = (String)m.get("Label");
					String style = (String)m.get("Style");
					createContextMenuAction(pif,manager,action,path,label,style);
				}
			} catch (PrologInterfaceException e) {
				Debug.rethrow(e);
			}finally{
				if(s!=null){
					s.dispose();
				}
			}
		}
		
		// Other plug-ins can contribute there actions here
		manager.add(new Separator(IWorkbenchActionConstants.MB_ADDITIONS));
	}

	private void createContextMenuAction(final PrologInterface pif,IMenuManager manager, final String action,
			String path, String label, String style) {
		IMenuManager where = manager.findMenuUsingPath(path);
		where=(where==null)?manager:where;
		if("menu".equals(style)){
			where.add(new MenuManager(label,action));
		}else if("action".equals(style)){
			where.add(new QueryAction(label, Action.AS_PUSH_BUTTON, pif, action));
		}
		
	}



	private String[] getSelectedIds(){
		IStructuredSelection selection = (IStructuredSelection) viewer.getSelection();
		String[] result = new String[selection.size()];
		int i=0;
		for(Iterator it = selection.iterator();it.hasNext();){
			PEFHandle handle = (PEFHandle) it.next();
			result[i++]=handle.getId();
		}
		return result;
	}
	
	private void fillLocalToolBar(IToolBarManager manager) {
		manager.add(clearAction);
		manager.add(setVisibilityAction);
		manager.add(selectPifAction);
		manager.add(refreshAction);
		// manager.add(action2);
	}

	private void makeActions() {
		clearAction = new Action() {
			@Override
			public void run() {
				PrologInterface pif = (PrologInterface) viewer.getInput(); 
				if (pif == null) {
					return;
				}
				PrologSession session = null;
				try {

					session = pif.getSession();
					session.queryOnce("pef_graph_clear");
					contentProvider.reset();
				} catch (PrologInterfaceException e) {
					Debug.rethrow(e);
				} finally {
					if (session != null) {
						session.dispose();
					}
				}
			}

		};
		clearAction.setText("Clear Graph");
		clearAction.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.CLEAR));

		setVisibilityAction = new SetVisibilityAction() {

			@Override
			protected PrologInterface getPrologInterface() {

				return (PrologInterface) viewer.getInput();
			}

			@Override
			protected Shell getShell() {

				return PEFGraphView.this.getSite().getShell();
			}

		};
		setVisibilityAction.setText("Modify Node Visibility");
		setVisibilityAction.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.SET_VISIBILITY));

		selectPifAction = new SelectPifAction() {

			@Override
			protected PrologInterface getPrologInterface() {
				return (PrologInterface) viewer.getInput();
			}

			@Override
			protected void setPrologInterface(PrologInterface prologInterface) {
				viewer.setInput(prologInterface);

			}

		};
		refreshAction = new Action() {
			

			@Override
			public void run() {
				contentProvider.reset();
			}
		};
		refreshAction.setImageDescriptor(ImageRepository
				.getImageDescriptor(ImageRepository.RESTART));
	}

	

	
	
	
	
	
	@Override
	public void setFocus() {
		// TODO Auto-generated method stub
		
	}



	public Viewer getViewer() {		
		return viewer;
	}



	

}