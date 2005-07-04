package org.cs3.jlmp.internal.actions;

import java.util.Map;

import org.cs3.jlmp.JLMP;
import org.cs3.jlmp.internal.natures.JLMPProjectNature;
import org.cs3.pl.common.Debug;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.Path;
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
	 */
	static PrologSession getPrologSession() throws CoreException {
		IProject[] projects = ResourcesPlugin.getWorkspace().getRoot().getProjects();
		JLMPProjectNature nature = null;
		for (int i = 0; i < projects.length; i++) {
			if(projects[i].isAccessible() && projects[i].hasNature(JLMP.NATURE_ID)){
				nature = (JLMPProjectNature)projects[i].getNature(JLMP.NATURE_ID);
				break;
			}
		}
		if(nature == null)
			return null;
		return nature.getPrologInterface().getSession();
	}
	
	public void run() {
		PrologSession session;
		try {
			session = getPrologSession();
			Map result = session.queryOnce("sourceLocation(" + getPefId()
					+ ", File, Start, Length)");
			if (result == null) {
				setStatusErrorMessage(
						"could not find source location for id '" + getPefId()
								+ "'.");
			}
			else {
				String filename = result.get("File").toString();
				int start = Integer.parseInt(result.get("Start").toString());
				int length = Integer.parseInt(result.get("Length").toString());
				selectInEditor(start, length, filename);
			}
		} catch (CoreException e) {
			e.printStackTrace();
			Debug.report(e);
		}
	}
	private void setStatusErrorMessage(final String string) {
		PlatformUI.getWorkbench().getDisplay().asyncExec(new Runnable() {
			public void run() {
				getActiveEditor().getEditorSite().getActionBars().getStatusLineManager().setErrorMessage(string);
			}
		});
	}
	public void selectInEditor(int start, int length, String filename) throws PartInitException {
		Path path = new Path(filename);
		IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path);
		if (file == null) {
			setStatusErrorMessage("could not find the file: '" + filename + "' in the workspace.");
			return;
		}
		openInEditor(file, true);
		IDocument document = ((AbstractTextEditor)getActiveEditor()).getDocumentProvider().getDocument(getActiveEditor().getEditorInput());
		ISelection selection = new TextSelection(document,start,length);
		getActiveEditor().getEditorSite().getSelectionProvider().setSelection(selection);
	}

	public IEditorPart openInEditor(IFile file, boolean activate) throws PartInitException {
		if (file != null) {
			IWorkbenchPage p= getActivePage();
			if (p != null) {
				IEditorPart editorPart= IDE.openEditor(p, file, activate);
				return editorPart;
			}
		}
		return null; 
	} 

	private IEditorPart getActiveEditor() {
		return getActivePage().getActiveEditor();
	}

	private IWorkbenchPage getActivePage() {
		return PlatformUI.getWorkbench().getActiveWorkbenchWindow().
		getActivePage();
	}

}
