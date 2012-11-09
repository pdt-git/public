package pdt.y.focusview;

import java.util.List;

import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.cs3.prolog.pif.PrologInterface;
import org.cs3.prolog.pif.PrologInterfaceException;
import org.cs3.prolog.pif.service.ConsultListener;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IProgressMonitor;

import pdt.y.preferences.MainPreferencePage;

public class GlobalViewCoordinator extends ViewCoordinatorBase implements ConsultListener {
	
	FocusView.FocusViewControl currentFocusView;

	public GlobalViewCoordinator(FocusView focusView)
	{
		super(focusView);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
		
	public FocusView.FocusViewControl swichFocusView(String path) {
		refreshCurrentView(path);
		return currentFocusView;
	}

	protected void refreshCurrentView(String path) {
		//if (currentFocusView == null) {
		currentFocusView = focusView.new FocusViewControl(path);
		focusView.setCurrentFocusView(currentFocusView);
		//}
		
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
		
		if (files.isEmpty())
			return;
		
		refreshCurrentView(files.get(0).toString());
	}
}