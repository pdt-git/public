package pdt.y.focusview;

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


public abstract class ViewCoordinatorBase implements IPartListener, ConsultListener  {
	
	protected FocusView focusView;
	protected FocusView.FocusViewControl currentFocusView;

	public ViewCoordinatorBase(FocusView focusView) {
		this.focusView = focusView;

		
		focusView.getSite().getWorkbenchWindow().getPartService().addPartListener(this);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
	public abstract void swichFocusView(String path);

	protected abstract boolean isCurrentFocusViewActualFor(String path);
	
	@Override
	public void partActivated(IWorkbenchPart part) {
		if (part instanceof IEditorPart) {
			IEditorPart editorPart = (IEditorPart) part;
			final String fileName = PDTCommonUtil.prologFileName(editorPart.getEditorInput());
			if (!fileName.endsWith(".pl") && !fileName.endsWith(".pro")) {
				return;
			}
			if (currentFocusView == null 
					|| !isCurrentFocusViewActualFor(fileName)) {
				
				new UIJob("Update View") {
					@Override
					public IStatus runInUIThread(IProgressMonitor monitor) {
						
						swichFocusView(fileName);
						
						if (currentFocusView.isEmpty()){
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
	
	@Override
	public void beforeConsult(PrologInterface pif, List<IFile> files,
			IProgressMonitor monitor) throws PrologInterfaceException { }

	@Override
	public void afterConsult(PrologInterface pif, List<IFile> files,
			List<String> allConsultedFiles, IProgressMonitor monitor)
			throws PrologInterfaceException {
		
		refreshCurrentView();
	}
	
	protected void refreshCurrentView() {
		if (MainPreferencePage.isAutomaticUpdate() && currentFocusView != null) {
			new UIJob("Update View") {
				@Override
				public IStatus runInUIThread(IProgressMonitor monitor) {
					currentFocusView.reload();
					return Status.OK_STATUS;
				}
			}.schedule();
		}
	}
}