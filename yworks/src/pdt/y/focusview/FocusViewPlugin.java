package pdt.y.focusview;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
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

import pdt.y.internal.ui.ToolBarAction;
import pdt.y.main.PluginActivator;
import pdt.y.main.PreferencesUpdateListener;
import pdt.y.preferences.EdgeAppearancePreferences;
import pdt.y.preferences.FileAppearancePreferences;
import pdt.y.preferences.PredicateAppearancePreferences;
import pdt.y.preferences.PredicateLayoutPreferences;
import pdt.y.preferences.PreferenceConstants;
import pdt.y.preferences.MainPreferencePage;



public class FocusViewPlugin extends ViewPart {

	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private Composite viewContainer;
	private Label info;
	private String infoText = "", statusText = "";
	
	public FocusViewPlugin() {
		PluginActivator.getDefault().addPreferencesUpdateListener(new PreferencesUpdateListener() {
			@Override
			public void preferencesUpdated() {
				updateCurrentFocusView();	
			}
		});
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
		
		initButtons(parent);
		
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

	protected void initButtons(final Composite parent) {
		IActionBars bars = this.getViewSite().getActionBars();
		IToolBarManager toolBarManager = bars.getToolBarManager();
		
		
		toolBarManager.add(new ToolBarAction("Update", "WARNING: Current layout will be rearranged!", 
				org.cs3.pdt.internal.ImageRepository.getImageDescriptor(
						org.cs3.pdt.internal.ImageRepository.REFRESH)) {

				@Override
				public void performAction() {
					updateCurrentFocusView();	
				}
			});
		
		toolBarManager.add(new ToolBarAction("Hierarchical layout", 
				pdt.y.internal.ImageRepository.getImageDescriptor(
						pdt.y.internal.ImageRepository.HIERARCHY)) {

				@Override
				public void performAction() {
					PredicateLayoutPreferences.setLayoutPreference(PreferenceConstants.LAYOUT_HIERARCHY);
					updateCurrentFocusViewLayout();
				}
			});
		
		toolBarManager.add(new ToolBarAction("Organic layout", 
				pdt.y.internal.ImageRepository.getImageDescriptor(
						pdt.y.internal.ImageRepository.ORGANIC)) {

				@Override
				public void performAction() {
					PredicateLayoutPreferences.setLayoutPreference(PreferenceConstants.LAYOUT_ORGANIC);
					updateCurrentFocusViewLayout();
				}
			});
		
		toolBarManager.add(new ToolBarAction("Preferences", 
				org.cs3.pdt.internal.ImageRepository.getImageDescriptor(
						org.cs3.pdt.internal.ImageRepository.PREFERENCES)) {

				@Override
				public void performAction() {
					PreferenceManager mgr = new PreferenceManager();
					
					IPreferencePage page = new MainPreferencePage();
					page.setTitle("Focus View");
					
					IPreferenceNode node = new PreferenceNode("PreferencePage", page);
					mgr.addToRoot(node);
					
					IPreferencePage edgePrefs = new EdgeAppearancePreferences();
					edgePrefs.setTitle("Edge Appearance");
					node.add(new PreferenceNode("EdgeAppearancePreferences", edgePrefs));
					
					IPreferencePage filePrefs = new FileAppearancePreferences();
					filePrefs.setTitle("File Appearance");
					node.add(new PreferenceNode("FileAppearancePreferences", filePrefs));
					
					IPreferencePage predicatePrefs = new PredicateAppearancePreferences();
					predicatePrefs.setTitle("Predicate Appearance");
					node.add(new PreferenceNode("PredicateAppearancePreferences", predicatePrefs));
					
					IPreferencePage predicateLayoutPrefs = new PredicateLayoutPreferences();
					predicateLayoutPrefs.setTitle("Predicate Layout");
					node.add(new PreferenceNode("PredicateLayoutPreferences", predicateLayoutPrefs));
					
					PreferenceDialog dialog = new PreferenceDialog(getSite().getShell(), mgr);
					dialog.create();
					dialog.setMessage(page.getTitle());
					dialog.open();
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
		FocusView f = getCurrentFocusView();
		if (f != null)
			f.reload();
	}
	
	public void updateCurrentFocusViewLayout() {
		FocusView f = getCurrentFocusView();
		if (f != null)
			f.updateLayout();
	}
	
	private FocusView getCurrentFocusView() {
		Control f = ((StackLayout)viewContainer.getLayout()).topControl;
		if (f instanceof FocusView)
			return (FocusView) f;
		return null;
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