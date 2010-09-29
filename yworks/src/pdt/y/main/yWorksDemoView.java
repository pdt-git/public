package pdt.y.main;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.ViewPart;

import pdt.y.swt.commands.GraphLoadAction;
import pdt.y.swt.commands.GraphPIFAction;


public class yWorksDemoView extends ViewPart {


	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private SwingControl swingControl;
    private PDTGraphSwing view = new PDTGraphSwing();

	public yWorksDemoView() {
	}

	@Override
	public void createPartControl(final Composite parent) {

		swingControl = new SwingControl(parent, SWT.NONE) {
			@Override
			protected JComponent createSwingComponent() {
				
				return view;
			}

			@Override
			public Composite getLayoutAncestor() {
				return parent;
			}
		};


		createMenu();
		createToolbar();
	}

		

  @Override
public void setFocus() {
    swingControl.setFocus();
  }


	  /**
     * Create menu.
     */
    private void createMenu() {
            
    }
    
    /**
     * Create toolbar.
     */
    private void createToolbar() {
            IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
            mgr.add(new GraphLoadAction(view));
            mgr.add(new GraphPIFAction(view));
    }


}