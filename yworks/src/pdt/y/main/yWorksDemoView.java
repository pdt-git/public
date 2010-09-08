package pdt.y.main;



import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.forms.widgets.Form;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.ViewPart;






public class yWorksDemoView extends ViewPart {


	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private SwingControl swingControl;
    private GraphPDTDemo view;
	private FormToolkit toolkit;
	private Form form;



	public yWorksDemoView() {
	}

	public void createPartControl(final Composite parent) {
		toolkit = new FormToolkit(parent.getDisplay());
		form = toolkit.createForm(parent);
		form.setText("Hello, Eclipse Forms");
		toolkit.decorateFormHeading(form);
		
		swingControl = new SwingControl(parent, SWT.NONE) {
		

			protected JComponent createSwingComponent() {
		    	view = new GraphPDTDemo();
		       
		        return view;
		      }

		      public Composite getLayoutAncestor() {
		        return parent;
		      }
		    };
		
		    createMenu();
		    createToolbar();
  }

		

  public void setFocus() {
    swingControl.setFocus();
  }


	  /**
     * Create menu.
     */
    private void createMenu() {
            IMenuManager mgr = getViewSite().getActionBars().getMenuManager();
            
    }
    
    /**
     * Create toolbar.
     */
    private void createToolbar() {
            IToolBarManager mgr = getViewSite().getActionBars().getToolBarManager();
                    
    }


}