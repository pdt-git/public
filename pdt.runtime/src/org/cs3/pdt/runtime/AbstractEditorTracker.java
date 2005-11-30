package org.cs3.pdt.runtime;

import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;

public abstract class AbstractEditorTracker extends AbstractPrologContextTracker implements IPartListener2
{

	public AbstractEditorTracker()
	{
		super();
	}

	public AbstractEditorTracker(String id, String label)
	{
		super(id, label);
	}

	public void init(IWorkbench workbench) {
		workbench.getActiveWorkbenchWindow().getPartService().addPartListener(this);
		fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef) {
		if(partRef instanceof IEditorReference){
			
				fireContextChanged();
			
		}
	}
	
	public void partActivated(IWorkbenchPartReference partRef) {		
		check(partRef);		
	}	

	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		check(partRef);	
	}

	public void partClosed(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partDeactivated(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partHidden(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partVisible(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partInputChanged(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	

}
