package pdt.y.focusview;

import static org.eclipse.ui.IWorkbenchCommandConstants.FILE_SAVE;
import static org.eclipse.ui.IWorkbenchCommandConstants.FILE_SAVE_ALL;

import java.util.HashMap;
import java.util.List;

import javax.swing.JComponent;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PDTChangedFileInformation;
import org.eclipse.albireo.core.SwingControl;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.preference.IPreferenceNode;
import org.eclipse.jface.preference.IPreferencePage;
import org.eclipse.jface.preference.PreferenceDialog;
import org.eclipse.jface.preference.PreferenceManager;
import org.eclipse.jface.preference.PreferenceNode;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolTip;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.part.ViewPart;
import org.eclipse.ui.progress.UIJob;

import pdt.y.internal.ui.ToolBarAction;
import pdt.y.main.PDTGraphView;
import pdt.y.main.PluginActivator;
import pdt.y.main.PreferencesUpdateListener;
import pdt.y.model.GraphDataHolder;
import pdt.y.preferences.EdgeAppearancePreferences;
import pdt.y.preferences.FileAppearancePreferences;
import pdt.y.preferences.MainPreferencePage;
import pdt.y.preferences.PredicateAppearancePreferences;
import pdt.y.preferences.PredicateLayoutPreferences;
import pdt.y.preferences.PreferenceConstants;
import y.base.Node;
import y.view.HitInfo;
import y.view.NodeLabel;
import y.view.ViewMode;


public class FocusViewPlugin extends ViewPart {

	public static final String ID = "pdt.yworks.swt.views.yWorksDemoView";
	private Composite viewContainer;
	private Label info;
	private String infoText = "", statusText = "";
	private FocusViewCoordinator focusViewCoordinator;
	
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
		
		focusViewCoordinator = new FocusViewCoordinator();
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
		        if (info.isDisposed()) {
		        	return Status.CANCEL_STATUS;
		        } else {
			    	info.setText(text);
			        return Status.OK_STATUS;
		        }
		    }
		}.schedule();
	}
	
	@Override
	public void dispose() {
		super.dispose();
		PDTPlugin.getDefault().removeSelectionChangedListener(focusViewCoordinator);
	}
	
	public class FocusView extends SwingControl {

		private final String FOCUS_VIEW_IS_OUTDATED = "[FocusView is outdated]";
		
		private final PDTGraphView pdtGraphView;
		private final GraphPIFLoader pifLoader;
		private final String filePath;
		
		private boolean isDirty = false;
		
		public FocusView(final String filePath) {
			super(viewContainer, SWT.NONE); 
			
			this.filePath = filePath;
			
			this.pdtGraphView = new PDTGraphView();
			
			this.pifLoader = new GraphPIFLoader(pdtGraphView);
			
			pdtGraphView.addViewMode(new OpenInEditorViewMode(pdtGraphView, pifLoader));
			pdtGraphView.addViewMode(new HoverTrigger(getShell()));
		}	
		
		public String getFilePath() {
			return filePath;
		}
		
		public boolean isEmpty() {
			return pdtGraphView.isEmpty();
		}
		
		public void setDirty() {
			FocusViewPlugin.this.setStatusText(FOCUS_VIEW_IS_OUTDATED);
			isDirty = true;
		}
		
		public boolean isDirty() {
			return isDirty;
		}
		
		public List<String> getDependencies() {
			return pifLoader.getDependencies();
		}

		public void reload() {
			Job j = new Job("PDT Focus View: Reloading Graph") {
				@Override
				protected IStatus run(IProgressMonitor monitor) {
					pifLoader.queryPrologForGraphFacts(filePath);
					FocusViewPlugin.this.setStatusText("");
					
					isDirty = false;

					return Status.OK_STATUS;
				}
			};
			j.schedule();
		}
		
		public void updateLayout() {
			pdtGraphView.updateLayout();
		}

		@Override
		protected JComponent createSwingComponent() {
			return pdtGraphView;
		}

		@Override
		public Composite getLayoutAncestor() {
			return viewContainer;
		}

		private final class HoverTrigger extends ViewMode {

			private final ToolTip t;
			
			public HoverTrigger(Shell parent) {
				t = new ToolTip(parent, SWT.NONE);
				t.setVisible(false);
			}
			
			public void mouseMoved(final double x, final double y) {
				super.mouseMoved(x, y);
				
				new UIJob("Updating status") {
					
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						updateStatus(x, y);
						return Status.OK_STATUS;
					}
				
				}.schedule();
			}

			protected void updateStatus(double x, double y) {
				HitInfo hitInfo = getHitInfo(x, y);
				
				if (hitInfo.hasHitNodes()) {
					Node node = hitInfo.getHitNode();
					NodeLabel label = pdtGraphView.getGraph2D().getRealizer(node).getLabel();
					
					StringBuilder sb = new StringBuilder(); 

					GraphDataHolder data = pdtGraphView.getDataHolder();

					if (data.isModule(node)) {
						sb.append("Module: ");
					} else if (data.isFile(node)) {
						sb.append("File: ");
					} else if (data.isPredicate(node)) {
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
					
					String text = sb.toString();

					FocusViewPlugin.this.setInfoText(text);
					
					if (MainPreferencePage.isShowToolTip() && text.startsWith("Predicate")) {
						Point location = Display.getCurrent().getCursorLocation();
						location.x += 10;
						location.y += 10;
						t.setLocation(location);
						t.setMessage(text.substring(11));
						t.setVisible(true);
					}
					else {
						t.setVisible(false);
					}
				} else {
					FocusViewPlugin.this.setInfoText("");
					t.setVisible(false);
				}
			}
		}
	}

	public class FocusViewCoordinator implements ISelectionChangedListener, IExecutionListener {
		
		final HashMap<String, FocusView> views = new HashMap<String, FocusView>();
		
		FocusView currentFocusView;

		public FocusViewCoordinator() {
			PDTPlugin.getDefault().addSelectionChangedListener(this);
			
			ICommandService service = (ICommandService) PlatformUI
				.getWorkbench().getService(ICommandService.class);
		    service.addExecutionListener(this);
		}
		
		@Override
		public void selectionChanged(SelectionChangedEvent event) {
			ISelection selection = event.getSelection();
			
			if (selection instanceof PDTChangedFileInformation) {
			
				final PDTChangedFileInformation fileInfo = (PDTChangedFileInformation)selection;
				
				if (currentFocusView == null 
						|| !currentFocusView.getFilePath().equals(fileInfo.getPrologFileName())) {
					
					new UIJob("Update Focus View") {
					    public IStatus runInUIThread(IProgressMonitor monitor) {
					    	
					    	FocusView f = swichFocusView(fileInfo.getPrologFileName());
					    	
					    	if (f.isEmpty()){
					    		FocusViewPlugin.this.setStatusText("[Please activate prolog console, set focus on file and press F9 to load graph]");
					    	}
					        
					    	return Status.OK_STATUS;
					    }
					}.schedule();
				}
				if (currentFocusView != null 
						&& currentFocusView.isEmpty()) {
					currentFocusView.reload();
				}
			}
		}
		
		private FocusView swichFocusView(String path) {
			currentFocusView = views.get(path);
			if (currentFocusView == null) {
				currentFocusView = new FocusView(path);

				views.put(path, currentFocusView);
			}
			
			FocusViewPlugin.this.setCurrentFocusView(currentFocusView);
			refreshCurrentView();
			
			return currentFocusView;
		}

		private void setDirtyFocusView(String location) {
			if (location == null)
				return;
			
			String path = location.toLowerCase().replace('\\', '/');
			if (!path.endsWith(".pl"))
				return;
			
			for (FocusView f : views.values()) {
				for (String d : f.getDependencies()) {
					if (path.equals(d)) {
						f.setDirty();
						break;
					}
				}
			}
		}

		protected void refreshCurrentView() {
			if (MainPreferencePage.isAutomaticUpdate()
					&& currentFocusView.isDirty()) {
				currentFocusView.reload();
			}
		}
		
		@Override
		public void notHandled(String commandId, NotHandledException exception) { }

		@Override
		public void postExecuteFailure(String commandId,
				ExecutionException exception) { }

		@Override
		public void preExecute(String commandId, ExecutionEvent event) { }
		
		@Override
		public void postExecuteSuccess(String commandId, Object returnValue) { 
			IWorkbenchPage wb = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
			if (commandId.equals(FILE_SAVE)) {
				setDirtyFocusView(getFileLocation(wb.getActiveEditor()));
			}
			else if (commandId.equals(FILE_SAVE_ALL)) {
				IEditorPart[] editors = wb.getEditors();
				for (IEditorPart e : editors) {
					setDirtyFocusView(getFileLocation(e));
				}
			}
			refreshCurrentView();
		}

		private String getFileLocation(IEditorPart editor) {
	        if (editor != null) {
	            IEditorInput input = editor.getEditorInput();
	            if (input instanceof IFileEditorInput) {
	                return ((IFileEditorInput)input).getFile().getLocation().toOSString();
	            }
	        }
	        return null;
		}
	}
}