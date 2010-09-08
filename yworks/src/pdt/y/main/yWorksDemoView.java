package pdt.y.main;



import javax.swing.JComponent;

import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.part.*;
import org.eclipse.albireo.core.SwingControl;
import org.eclipse.jface.viewers.*;
import org.eclipse.swt.graphics.Image;
import org.eclipse.jface.action.*;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.*;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.SWT;

import y.view.EditMode;
import y.view.Graph2DView;


/**
 * This sample class demonstrates how to plug-in a new
 * workbench view. The view shows data obtained from the
 * model. The sample creates a dummy model on the fly,
 * but a real implementation would connect to the model
 * available either in this or another plug-in (e.g. the workspace).
 * The view is connected to the model using a content provider.
 * <p>
 * The view uses a label provider to define how model
 * objects should be presented in the view. Each
 * view can present the same model objects using
 * different labels and icons, if needed. Alternatively,
 * a single label provider can be shared between views
 * in order to ensure that objects of the same type are
 * presented in the same way everywhere.
 * <p>
 */

public class yWorksDemoView extends ViewPart {

	/**
	 * The ID of the view as specified by the extension.
	 */
	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private SwingControl swingControl;


	/**
	 * The constructor.
	 */
	public yWorksDemoView() {
	}

	/**
	 * This is a callback that will allow us
	 * to create the viewer and initialize it.
	 */
	public void createPartControl(final Composite parent) {
		swingControl = new SwingControl(parent, SWT.NONE) {
		      protected JComponent createSwingComponent() {
		    	GraphPDTDemo view = new GraphPDTDemo();
		       
		        return view;
		      }

		      public Composite getLayoutAncestor() {
		        return parent;
		      }
		    };
		  }

		

  public void setFocus() {
    swingControl.setFocus();
  }
	private void contributeToActionBars() {
		IActionBars bars = getViewSite().getActionBars();
		fillLocalPullDown(bars.getMenuManager());
		fillLocalToolBar(bars.getToolBarManager());
	}

	private void fillLocalToolBar(IToolBarManager toolBarManager) {
		
		
	}

	private void fillLocalPullDown(IMenuManager menuManager) {
		// TODO Auto-generated method stub
		
	}


}