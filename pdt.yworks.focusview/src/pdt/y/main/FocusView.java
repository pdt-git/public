package pdt.y.main;

import java.awt.event.MouseEvent;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.part.ViewPart;

import org.eclipse.ui.progress.UIJob;

import pdt.y.model.GraphDataHolder;
import pdt.y.model.GraphModel;

import y.base.Node;
import y.view.HitInfo;
import y.view.NodeLabel;
import y.view.ViewMode;


public class FocusView extends ViewPart {


	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private SwingControl swingControl;
	private PDTGraphSwingStandalone view = new PDTGraphSwingStandalone();
	private Label info;
	private GraphPIFCoordinator pifCoordinator;

	public FocusView() {
	}

	@Override
	public void createPartControl(final Composite parent) {
		FormLayout layout = new FormLayout();
		parent.setLayout(layout);
		
		swingControl = new SwingControl(parent, SWT.NONE) {
			@Override
			protected JComponent createSwingComponent() {
				pifCoordinator = new GraphPIFCoordinator(view);
				//GraphDataHolder dataHolder = view.getDataHolder();
				view.addViewMode(new OpenInEditorViewMode(view, pifCoordinator));
				
				HoverTrigger hoverTrigger = new HoverTrigger(view);
			    view.addViewMode(hoverTrigger);
				
			    return view;
			}

			@Override
			public Composite getLayoutAncestor() {
				return parent;
			}
		};
		
		FormData swingControlLD = new FormData();
		swingControlLD.left = new FormAttachment(0, 0);
		swingControlLD.right = new FormAttachment(100, 0);
		swingControlLD.top = new FormAttachment(0, 0);
		swingControlLD.bottom = new FormAttachment(100, -25);
		swingControl.setLayoutData(swingControlLD);
		
		info = new Label(parent, SWT.NONE);
		
		FormData infoLD = new FormData();
		infoLD.left = new FormAttachment(0, 5);
		infoLD.top = new FormAttachment(swingControl, 3);
		infoLD.right = new FormAttachment(100, 0);
		info.setLayoutData(infoLD);
		
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
	
	public void setInfo(final String text) {
		UIJob uiJob = new UIJob("Update Status") {
		    public IStatus runInUIThread(IProgressMonitor monitor) {
		        info.setText(text);
		        return Status.OK_STATUS;
		    }
		};
		uiJob.schedule();
	}
	
	private final class HoverTrigger extends ViewMode {
		
		PDTGraphSwingStandalone pdtGraphView;
			
		public HoverTrigger(PDTGraphSwingStandalone pdtGraphView) {
			this.pdtGraphView = pdtGraphView;
		}

		public void mouseMoved(double x, double y) {
			super.mouseMoved(x, y);
			HitInfo hitInfo = getHitInfo(x, y);
			if (hitInfo.hasHitNodes()) {
			    Node node = hitInfo.getHitNode();
			    NodeLabel label = view.getGraph2D().getRealizer(node).getLabel();
			    StringBuilder sb = new StringBuilder();
			    
			    GraphDataHolder data = pdtGraphView.getDataHolder();
			    
			    if (data.isModule(node)) {
			    	sb.append("Module: ");
			    } 
			    else if(data.isFile(node)) {
			    	sb.append("File: ");
			    }
			    else if (data.isPredicate(node)) {
			    	sb.append("Predicate: ");
			    }
			    
			    sb.append(label.getText());
			    
			    if (data.isExported(node)) {
			    	sb.append(" [Exported]");
			    }
			    
			    if (data.isDynamicNode(node)) {
			    	sb.append(" [Dynamic]");
			    }
			    
			    if (data.isUnusedLocal(node)) {
			    	sb.append(" [Unused]");
			    }
			    
			    setInfo(sb.toString());
			} else {
				setInfo("");
		  	}
	    }
	}
}