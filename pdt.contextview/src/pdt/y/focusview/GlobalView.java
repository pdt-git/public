package pdt.y.focusview;


public class GlobalView extends FocusView {
	
	@Override
	protected ViewCoordinatorBase createViewCoordinator() {
		return new GlobalViewCoordinator(this);
	}
}
