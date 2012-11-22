package pdt.y.focusview;

import java.util.HashMap;

import pdt.y.main.PDTGraphView;

public class FocusViewCoordinator extends ViewCoordinatorBase {
	
	final HashMap<String, FocusView.FocusViewControl> views = new HashMap<String, FocusView.FocusViewControl>();
	
	public FocusViewCoordinator(FocusView focusView) {
		super(focusView);
	}
	
	public void swichFocusView(String path) {
		currentFocusView = views.get(path);
		if (currentFocusView == null) {
			PDTGraphView pdtGraphView = new PDTGraphView();
			FocusGraphPIFLoader loader = new FocusGraphPIFLoader(pdtGraphView);
			loader.setFocusFile(path);
			
			currentFocusView = focusView.new FocusViewControl(pdtGraphView, loader);

			refreshCurrentView();
			
			views.put(path, currentFocusView);
		}
		
		focusView.setCurrentFocusView(currentFocusView);
	}

	@Override
	protected boolean isCurrentFocusViewActualFor(String path) {
		return views.containsKey(path) && views.get(path) == currentFocusView;
	}
}