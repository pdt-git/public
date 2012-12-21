package pdt.y.focusview;

import java.util.HashMap;

import pdt.y.main.PDTGraphView;

public class FocusViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, ViewBase.FocusViewControl> views = new HashMap<String, ViewBase.FocusViewControl>();
	
	public FocusViewCoordinator(ViewBase focusView) {
		super(focusView);
	}
	
	public void swichFocusView(String path) {
		currentFocusView = views.get(path);
		if (currentFocusView == null) {
			PDTGraphView pdtGraphView = new PDTGraphView(focusView);
			GraphPIFLoaderBase loader = focusView.createGraphPIFLoader(pdtGraphView);
			loader.setCurrentPath(path);
			
			currentFocusView = focusView.createFocusViewControl(pdtGraphView, loader);

			refreshCurrentView();
			
			views.put(path, currentFocusView);
		}
		
		currentFocusView.recalculateMode();
		focusView.setCurrentFocusView(currentFocusView);
	}

	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return views.containsKey(path) && views.get(path) == currentFocusView;
	}
}