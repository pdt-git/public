package org.cs3.jtransformer.internal.tracker;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.pdt.runtime.AbstractEditorTracker;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterface;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.ui.JavaUI;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;

public class JavaEditorTracker extends AbstractEditorTracker
{

	public JavaEditorTracker()
	{
		super();
	}

	public JavaEditorTracker(String id, String label)
	{
		super(id, label);
	}

	public PrologInterface getCurrentPrologInterface() {
		IEditorPart editor = UIUtils.getActiveEditor();

		if(editor == null)
		{
			return null;
		}
		if (!editor.getEditorSite().getId().equals(JavaUI.ID_CU_EDITOR)) {			
			return null;
		}
		IEditorInput input = editor.getEditorInput();
		IFileEditorInput fileInput=null;
		if (input instanceof IFileEditorInput) {			
			fileInput = (IFileEditorInput) input;			
		}
		if(fileInput==null){
			return null;
		}
		IProject project = fileInput.getFile().getProject();
		JTransformerProjectNature jtNature =null;
		try {
			if(project.hasNature(JTransformer.NATURE_ID)){
				jtNature=(JTransformerProjectNature)project.getNature(JTransformer.NATURE_ID);
			}
		} catch (CoreException e) {
			Debug.report(e);
			throw new RuntimeException(e);
		}
		if(jtNature==null){
			return null;
		}
		
		return jtNature.getPrologInterface();
	}

}
