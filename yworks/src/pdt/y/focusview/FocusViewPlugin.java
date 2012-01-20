package pdt.y.focusview;


import org.cs3.pdt.internal.ImageRepository;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;



public class FocusViewPlugin extends ViewPart {

	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private Composite viewContainer;
	private Label info;
	private String infoText = "", statusText = "";
	
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
		
		initGraphNotLoadedLabel();
		
		initInfoLabel(parent);
		
		initUpdateButton(parent);
		
		new FocusViewCoordinator(this, viewContainer);
	}

	protected void initGraphNotLoadedLabel() {
		// Temporal label initialization
		Label l = new Label(viewContainer, SWT.NONE);
		l.setText("Graph is not loaded");
		((StackLayout)viewContainer.getLayout()).topControl = l;
		viewContainer.layout();
	}

	protected void initInfoLabel(final Composite parent) {
		// Info label initialization
		info = new Label(parent, SWT.NONE);
		
		FormData infoLD = new FormData();
		infoLD.left = new FormAttachment(0, 5);
		infoLD.top = new FormAttachment(viewContainer, 3);
		infoLD.right = new FormAttachment(100, 0);
		info.setLayoutData(infoLD);
	}

	protected void initUpdateButton(final Composite parent) {
		IActionBars bars = this.getViewSite().getActionBars();
		IToolBarManager toolBarManager = bars.getToolBarManager();
		
		toolBarManager.add(new Action() {
			@Override
			public void run(){
					new UIJob("Updating graph")
					{
						@Override
						public IStatus runInUIThread(IProgressMonitor monitor) {
							updateCurrentFocusView();
							return Status.OK_STATUS;
					}
				}.schedule();
			}
			
			@Override
			public ImageDescriptor getImageDescriptor() {
				return ImageRepository.getImageDescriptor(ImageRepository.REFRESH);
			}
			
			@Override
			public String getText() {
				return "Update";
			}
			
			@Override
			public String getToolTipText() {
				return "WARNING: Current layout will be rearranged!";
			}
		});
	}
	
	@Override
	public void setFocus() {
		viewContainer.setFocus();
	}

	public Composite getViewContainer() {
		return viewContainer;
	}

	public void setCurrentFocusView(FocusView focusView) {
		((StackLayout)viewContainer.getLayout()).topControl = focusView;
		viewContainer.layout();
	}
	
	public void updateCurrentFocusView() {
		Control f = ((StackLayout)viewContainer.getLayout()).topControl;
		if (f instanceof FocusView)
			((FocusView)f).reload();
	}
	
	public String getInfoText() {
		return infoText;
	}
	
	public void setInfoText(String text) {
		infoText = text;
		updateInfo();
	}

	public String getStatusText() {
		return statusText;
	}
	
	public void setStatusText(String text) {
		statusText = text;
		updateInfo();
	}
	
	protected void updateInfo() {
		final String text = statusText + " " + infoText;
		new UIJob("Update Status") {
		    public IStatus runInUIThread(IProgressMonitor monitor) {
		        info.setText(text);
		        return Status.OK_STATUS;
		    }
		}.schedule();
	}
}