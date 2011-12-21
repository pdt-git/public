package pdt.y.main;

import static org.eclipse.ui.IWorkbenchCommandConstants.FILE_SAVE;
import static org.eclipse.ui.IWorkbenchCommandConstants.FILE_SAVE_ALL;

import java.util.HashMap;

import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.internal.editors.PDTChangedFileInformation;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.progress.UIJob;

public class FocusViewCoordinator implements ISelectionChangedListener, IExecutionListener {
	
	final FocusViewPlugin focusViewPlugin;
	final Composite viewContainer;
	final HashMap<String, FocusView> views = new HashMap<String, FocusView>();
	
	FocusView currentFocusView;

	public FocusViewCoordinator(final FocusViewPlugin plugin, final Composite viewContainer) {
		this.focusViewPlugin = plugin; 
		this.viewContainer = viewContainer;
		
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
				    		focusViewPlugin.setInfo("Please activate prolog console, set focus on file and press F9 to load graph...");
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
			currentFocusView = new FocusView(focusViewPlugin, viewContainer, path);

			views.put(path, currentFocusView);
		}
		
		focusViewPlugin.setCurrentFocusView(currentFocusView);
		
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
			return;
		}
		if (commandId.equals(FILE_SAVE_ALL)) {
			IEditorPart[] editors = wb.getEditors();
			for (IEditorPart e : editors) {
				setDirtyFocusView(getFileLocation(e));
			}
		}
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
