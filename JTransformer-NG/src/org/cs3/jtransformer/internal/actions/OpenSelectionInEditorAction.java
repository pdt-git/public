package org.cs3.jtransformer.internal.actions;

import java.util.Map;

import org.cs3.jtransformer.JTransformer;
import org.cs3.jtransformer.internal.natures.JTransformerProjectNature;
import org.cs3.jtransformer.util.JTUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologInterfaceException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.text.IDocument;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.texteditor.AbstractTextEditor;


public class OpenSelectionInEditorAction extends ConsoleSelectionAction{

	/**
	 * @throws CoreException
	 * @throws PrologInterfaceException 
	 */
	static PrologSession getPrologSession() throws CoreException, PrologInterfaceException {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		JTransformerProjectNature nature = null;
		for (int i = 0; i < projects.length; i++) {
			if(projects[i].isAccessible() && projects[i].hasNature(JTransformer.NATURE_ID)){
				nature = (JTransformerProjectNature)projects[i].getNature(JTransformer.NATURE_ID);
				break;
			}
		}
		if(nature == null)
			return null;
		return nature.getPrologInterface().getSession();
	}
	
	public void run(IAction action) {
		PrologSession session;
		try {
			session = getPrologSession();
			Map result = session.queryOnce("sourceLocation(" + getPefId()
					+ ", File, Start, Length)");
			if (result == null) {
				JTUtils.setStatusErrorMessage(
						"could not find source location for id '" + getPefId()
								+ "'.");
			}
			else {
				String filename = result.get("File").toString();
				int start = Integer.parseInt(result.get("Start").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				JTUtils.selectInEditor(start, length, filename);
			}
		} catch (CoreException e) {
			Debug.report(e);
		} catch (PrologInterfaceException e)
		{
			Debug.report(e);
		}
	}


}
