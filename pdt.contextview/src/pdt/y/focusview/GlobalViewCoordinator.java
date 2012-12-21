package pdt.y.focusview;

import java.io.IOException;
import java.util.HashMap;

import org.cs3.prolog.common.FileUtils;
import org.cs3.prolog.connector.ui.PrologRuntimeUIPlugin;
import org.eclipse.core.resources.IProject;

import pdt.y.main.PDTGraphView;

public class GlobalViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public GlobalViewCoordinator(ViewBase focusView)
	{
		super(focusView);
		
		PrologRuntimeUIPlugin.getDefault().getPrologInterfaceService()
			.registerConsultListener(this);
	}
	
		
	public void swichFocusView(String path) {
		try {
			IProject project = FileUtils.findFileForLocation(path).getProject();
		
			currentFocusView = views.get(project.getName());
			
			if (currentFocusView == null) {
				PDTGraphView pdtGraphView = new PDTGraphView(focusView);
				GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
				loader.setCurrentPath(path);
				
				currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);
	
				refreshCurrentView();
				
				views.put(project.getName(), currentFocusView);
			}
	
			currentFocusView.recalculateMode();
			focusView.setCurrentFocusView(currentFocusView);

		} catch (IOException e) {
			e.printStackTrace();
		}
	}


	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return currentFocusView != null 
				&& ((GlobalGraphPIFLoader)currentFocusView.getPifLoader()).containsFilePath(path);
	}
}