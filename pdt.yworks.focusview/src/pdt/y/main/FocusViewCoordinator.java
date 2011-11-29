package pdt.y.main;

import java.util.HashMap;

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
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.StackLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IWorkbenchCommandConstants;
import org.eclipse.ui.progress.UIJob;

import pdt.y.model.GraphDataHolder;
import y.base.Node;
import y.view.HitInfo;
import y.view.NodeLabel;
import y.view.ViewMode;

public class FocusViewCoordinator implements ISelectionChangedListener, IExecutionListener {
	
	FocusViewPlugin focusViewPlugin;
	Composite viewContainer;
	HashMap<String, FocusView> views = new HashMap<String, FocusView>();
	
	FocusView currentFocusView;

	public FocusViewCoordinator(FocusViewPlugin view, Composite viewContainer) {
		focusViewPlugin = view;
		this.viewContainer = viewContainer; 
		PDTPlugin.getDefault().addSelectionChangedListener(this);
	}
	
	@Override
	public void selectionChanged(SelectionChangedEvent event) {
		ISelection selection = event.getSelection();
		if (selection instanceof PDTChangedFileInformation) {
			final PDTChangedFileInformation fileInfo = (PDTChangedFileInformation)selection;
			if (currentFocusView == null 
					|| !currentFocusView.getFilePath().equals(fileInfo.getPrologFileName())) {
				new UIJob("Add Focus View") {
				    public IStatus runInUIThread(IProgressMonitor monitor) {
				    	swichFocusView(fileInfo.getPrologFileName());
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
	
	private Composite swichFocusView(String path) {
		currentFocusView = views.get(path);
		if (currentFocusView == null) {
			currentFocusView = new FocusView(focusViewPlugin, viewContainer, path);
			views.put(path, currentFocusView);
		}
		
		focusViewPlugin.setCurrentFocusView(currentFocusView);
		
		return currentFocusView;
	}

	private void setDirtyFocusView(String path) {
		// TODO implement logic
	}

	@Override
	public void notHandled(String commandId, NotHandledException exception) { }

	@Override
	public void postExecuteFailure(String commandId,
			ExecutionException exception) { }

	@Override
	public void postExecuteSuccess(String commandId, Object returnValue) {
		if (commandId.equals(IWorkbenchCommandConstants.FILE_SAVE)
				|| commandId.equals(IWorkbenchCommandConstants.FILE_SAVE_ALL)) {
			// TODO: setDirtyFocusView(path);
		}
	}

	@Override
	public void preExecute(String commandId, ExecutionEvent event) { 
		// TODO: get path
	}
}
