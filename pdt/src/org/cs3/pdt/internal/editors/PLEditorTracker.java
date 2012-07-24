/* $LICENSE_MSG$(ld) */

package org.cs3.pdt.internal.editors;

import org.cs3.prolog.connector.ui.AbstractPrologContextTracker;
import org.cs3.prolog.pif.PrologInterface;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IPartService;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.IWorkbenchWindow;


public class PLEditorTracker extends AbstractPrologContextTracker implements IPartListener2 {

	

	

	@Override
	public PrologInterface getCurrentPrologInterface(){
		return null;
	}

	@Override
	public void init(IWorkbench workbench) {
		IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
		IPartService partService = activeWorkbenchWindow.getPartService();
		partService.addPartListener(this);
		//FIXME: TRHO: Do not fire context changed on startup. Results in an unavailable prolog interface although another tracker was successful! 
		//fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef)  {
		if(partRef instanceof IEditorReference){
			if(getCurrentPrologInterface()!=null){
				fireContextChanged();
			}
		}
	}
	
	@Override
	public void partActivated(IWorkbenchPartReference partRef) {		
		check(partRef);		
	}	

	@Override
	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		check(partRef);	
	}

	@Override
	public void partClosed(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partDeactivated(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partHidden(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	@Override
	public void partVisible(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	@Override
	public void partInputChanged(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	

}

