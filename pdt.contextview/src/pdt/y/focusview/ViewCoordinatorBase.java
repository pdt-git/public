package pdt.y.focusview;


public abstract class ViewCoordinatorBase  {
	
	FocusView focusView;

	public ViewCoordinatorBase(FocusView focusView) {
		this.focusView = focusView;
	}
	
	public abstract FocusView.FocusViewControl swichFocusView(String path);
}