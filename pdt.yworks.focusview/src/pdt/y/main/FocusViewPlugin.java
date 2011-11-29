package pdt.y.main;

import java.awt.event.MouseEvent;

import javax.swing.JComponent;

import org.eclipse.albireo.core.SwingControl;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
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
import y.view.ViewCoordDrawableAdapter;
import y.view.ViewMode;


public class FocusViewPlugin extends ViewPart {

	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private Composite viewContainer;
	private Label info;
	
	public FocusViewPlugin() {
	}

	@Override
	public void createPartControl(final Composite parent) {
		FormLayout layout = new FormLayout();
		parent.setLayout(layout);
		
		// View container initialization
		viewContainer = new Composite(parent, SWT.NONE);
		viewContainer.setLayout(new StackLayout());
		
		FormData viewContainerLD = new FormData();
		viewContainerLD.left = new FormAttachment(0, 0);
		viewContainerLD.right = new FormAttachment(100, 0);
		viewContainerLD.top = new FormAttachment(0, 0);
		viewContainerLD.bottom = new FormAttachment(100, -25);
		viewContainer.setLayoutData(viewContainerLD);
		
		// Temporal label initialization
		Label l = new Label(viewContainer, SWT.NONE);
		l.setText("Graph is not loaded");
		((StackLayout)viewContainer.getLayout()).topControl = l;
		viewContainer.layout();
		
		// Info label initialization
		info = new Label(parent, SWT.NONE);
		
		FormData infoLD = new FormData();
		infoLD.left = new FormAttachment(0, 5);
		infoLD.top = new FormAttachment(viewContainer, 3);
		infoLD.right = new FormAttachment(100, 0);
		info.setLayoutData(infoLD);
		
		new FocusViewCoordinator(this, viewContainer);
	}

	public Composite getViewContainer() {
		return viewContainer;
	}

	public void setCurrentFocusView(Composite focusView) {
		((StackLayout)viewContainer.getLayout()).topControl = focusView;
		viewContainer.layout();
	}

	@Override
	public void setFocus() {
		viewContainer.setFocus();
	}
	
	public void setInfo(final String text) {
		new UIJob("Update Status") {
		    public IStatus runInUIThread(IProgressMonitor monitor) {
		        info.setText(text);
		        return Status.OK_STATUS;
		    }
		}.schedule();
	}
}