package pdt.y.focusview;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IToolBarManager;

import pdt.y.internal.ImageRepository;
import pdt.y.internal.ui.ToolBarAction;

public abstract class CallGraphViewBase extends ViewBase {
	private boolean metapredicateCallsVisible = true;
	private boolean inferredCallsVisible = true;

	public boolean isMetapredicateCallsVisible() {
		return metapredicateCallsVisible;
	}
	
	public boolean isInferredCallsVisible() {
		return inferredCallsVisible;
	}
	
	@Override
	protected void initViewButtons(IToolBarManager toolBarManager) {
		super.initViewButtons(toolBarManager);
		
		toolBarManager.add(new ToolBarAction("Show Calls To Metapredicates",
				ImageRepository.getImageDescriptor(ImageRepository.M)) {
				{
					setChecked(metapredicateCallsVisible);
				}
			
				@Override
				public int getStyle() {
					return IAction.AS_CHECK_BOX;
				}
				
				@Override
				public void performAction() {
					metapredicateCallsVisible = !metapredicateCallsVisible;
					updateCurrentFocusView();	
				}
			});
		
		toolBarManager.add(new ToolBarAction("Show Inferred Calls",
				ImageRepository.getImageDescriptor(ImageRepository.I)) {
				{
					setChecked(inferredCallsVisible);
				}
				
				@Override
				public int getStyle() {
					return IAction.AS_CHECK_BOX;
				}
			
				@Override
				public void performAction() {
					inferredCallsVisible = !inferredCallsVisible;
					updateCurrentFocusView();	
				}
			});
	}
}
