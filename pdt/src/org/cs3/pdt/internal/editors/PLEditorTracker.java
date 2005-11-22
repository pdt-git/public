package org.cs3.pdt.internal.editors;

import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.runtime.AbstractPrologContextTracker;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;


public class PLEditorTracker extends AbstractPrologContextTracker implements IPartListener2 {

	

	

	public PrologInterface getCurrentPrologInterface() {
		IEditorPart editor = UIUtils.getActiveEditor();
		
		PLEditor plEditor=null;
		if (editor instanceof PLEditor) {			
			plEditor = (PLEditor) editor;
		}
		if(plEditor==null){
			return null;
		}
		IEditorInput input = plEditor.getEditorInput();
		IFileEditorInput fileInput=null;
		if (input instanceof IFileEditorInput) {			
			fileInput = (IFileEditorInput) input;			
		}
		if(fileInput==null){
			return null;
		}
		IProject project = fileInput.getFile().getProject();
		IPrologProject plProject=null;
		try {
			if(project.hasNature(PDTCore.NATURE_ID)){
				plProject=(IPrologProject) project.getNature(PDTCore.NATURE_ID);
			}
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		if(plProject==null){
			return null;
		}
		
		return plProject.getRuntimePrologInterface();
	}

	public void init(IWorkbench workbench) {
		workbench.getActiveWorkbenchWindow().getPartService().addPartListener(this);
		fireContextChanged();
	}
	
	private void check(IWorkbenchPartReference partRef) {
		if(partRef instanceof IEditorReference){
			if(getCurrentPrologInterface()!=null){
				fireContextChanged();
			}
		}
	}
	
	public void partActivated(IWorkbenchPartReference partRef) {		
		check(partRef);		
	}	

	public void partBroughtToTop(IWorkbenchPartReference partRef) {
		check(partRef);	
	}

	public void partClosed(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	public void partDeactivated(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	public void partOpened(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partHidden(IWorkbenchPartReference partRef) {
		;//check(partRef);
	}

	public void partVisible(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	public void partInputChanged(IWorkbenchPartReference partRef) {
		check(partRef);
	}

	

}
