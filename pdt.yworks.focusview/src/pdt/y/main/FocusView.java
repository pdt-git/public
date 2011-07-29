package pdt.y.main;

import javax.swing.JComponent;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PDTChangedFileInformation;
import org.eclipse.albireo.core.SwingControl;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.internal.Workbench;
import org.eclipse.ui.part.ViewPart;

import pdt.y.view.swt.commands.GraphLoadAction;


public class FocusView extends ViewPart {


	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private SwingControl swingControl;
	private PDTGraphSwingStandalone view = new PDTGraphSwingStandalone();
	private GraphPIFCoordinator pifCoordinator;

	public FocusView() {
	}

	@Override
	public void createPartControl(final Composite parent) {

		swingControl = new SwingControl(parent, SWT.NONE) {
			@Override
			protected JComponent createSwingComponent() {
				pifCoordinator = new GraphPIFCoordinator(view);
				return view;
			}

			@Override
			public Composite getLayoutAncestor() {
				return parent;
			}
		};

		//createMenu();
		//createToolbar();
	}


	//public void registerSelectionChangedListener() {
	//	IEditorPart editor = Workbench.getInstance().getActiveWorkbenchWindow().getActivePage().getActiveEditor();
	//}

	@Override
	public void setFocus() {
		swingControl.setFocus();
	}


	//
	//	  /**
	//     * Create menu.
	//     */
	//    private void createMenu() {
	//            
	//    }
	//    
	//    /**
	//     * Create toolbar.
	//     */
	//    private void createToolbar() {
	//            IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
	//            mgr.add(new GraphLoadAction(view));
	//            mgr.add(new GraphPIFAction(view));
	//    }
	//

}