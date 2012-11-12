package pdt.y.focusview;

import java.util.HashMap;
import java.util.List;

import org.cs3.pdt.common.PDTCommonUtil;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ConsultListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.progress.UIJob;

import pdt.y.preferences.MainPreferencePage;

public class FocusViewCoordinator implements IPartListener, ConsultListener {
	
	final HashMap<String, FocusView.FocusViewControl> views = new HashMap<String, FocusView.FocusViewControl>();
	
	FocusView focusView;
	FocusView.FocusViewControl currentFocusView;

	public FocusViewCoordinator(FocusView focusView) {
		
		this.focusView = focusView;
		focusView.getSite().getWorkbenchWindow().getPartService().addPartListener(this);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			IEditorPart editorPart = (IEditorPart) part;
			final String fileName = PDTCommonUtil.prologFileName(editorPart.getEditorInput());
			if (!fileName.endsWith(".pl") && !fileName.endsWith(".pro")) {
				return;
			}
			if (currentFocusView == null 
					|| !currentFocusView.getFilePath().equals(fileName)) {
				
				new UIJob("Update Context View") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						
						FocusView.FocusViewControl f = swichFocusView(fileName);
						
						if (f.isEmpty()){
							focusView.setStatusText("[Please activate prolog console, set focus on file and press F9 to load graph]");
						}
						
						return Status.OK_STATUS;
					}
				}.schedule();
			}
		}
	}

	@Override
	public void partBroughtToTop(IWorkbenchPart part) {
	}

	@Override
	public void partClosed(IWorkbenchPart part) {
	}

	@Override
	public void partDeactivated(IWorkbenchPart part) {
	}

	@Override
	public void partOpened(IWorkbenchPart part) {
	}
	
	public FocusView.FocusViewControl swichFocusView(String path) {
		currentFocusView = views.get(path);
		if (currentFocusView == null) {
			currentFocusView = focusView.new FocusViewControl(path);

			views.put(path, currentFocusView);
		}
		
		focusView.setCurrentFocusView(currentFocusView);
		refreshCurrentView();
		
		return currentFocusView;
	}

	protected void refreshCurrentView() {
		if (MainPreferencePage.isAutomaticUpdate() && currentFocusView != null) {
			currentFocusView.reload();
		}
	}

	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files,
			IProgressMonitor monitor) throws PrologInterfaceException { }

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files,
			List<String> allConsultedFiles, IProgressMonitor monitor)
			throws PrologInterfaceException {
		
		refreshCurrentView();
	}
}