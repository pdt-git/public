package org.cs3.pl.buttons;

import java.io.File;
import java.io.IOException;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologHelper;
import org.cs3.pl.prolog.PrologManager;
import org.cs3.pl.prolog.SourceLocation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditorMessages;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.window.Window;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.dialogs.ElementListSelectionDialog;
import org.eclipse.ui.internal.editors.text.JavaFileEditorInput;
import org.eclipse.ui.part.FileEditorInput;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;


/**
 * @see IWorkbenchWindowActionDelegate
 */
public class FindPredicateActionDelegate extends TextEditorAction {
	private ITextEditor editor;
	final PDTPlugin plugin;
	
	/**
	 *
	 */
	public FindPredicateActionDelegate(ITextEditor editor) {
		super(JavaEditorMessages.getResourceBundle(), null, editor);
		this.editor =editor; 
		plugin = PDTPlugin.getDefault();
	}
	
	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	public void run() {
		//		plugin.getDisplay().asyncExec(new Runnable() {
		try {
			final PrologElementData data = ((PLEditor) plugin.getActiveEditor())
					.getSelectedPrologElement();
			if (data == null) {
				MessageDialog.openInformation(
						editor.getEditorSite().getShell(), "PDT Plugin",
						"Cannot locate a predicate at the specified location.");
				return;
			}
			Thread t = new Thread() { // fork from GUI thread
				public void run() {
					try {
						IPrologClient session = PrologManager.getInstance()
								.getHiddenClient();
						//if (!manager.isInCall()) {
							final SourceLocation location = getLocationForCurrentPredicateInEditor(data, session);
							plugin.getDisplay().syncExec(new Runnable() {
								public void run() {
									if (location == null) {
										MessageDialog
												.openInformation(
														editor.getEditorSite()
																.getShell(),
														"PDT Plugin",
														"Can't find predicate: " + data.getLabel()
														+ "/" + data.getArity() + ".\nProbably it is a build in(TODO).");
										return;
									}
									plugin.gotoLocation(location);
								}
							});
						//} else {
						//	plugin.setStatusErrorMessage("Cannot query the Prolog Prozess. It is blocked by another operation.");
						//	//manager.stop();
						//}
					} catch (IOException e) {
						Debug.report(e);
					}
				}
			};
			t.start();
			//		);
		} catch (BadLocationException ex) {
			PDTPlugin.getDefault().setStatusErrorMessage(
					"Can not find a valid predicate.");
		}
	}
		
	public void dispose() {
//		colorManager.dispose();
//		super.dispose();
	}
	
	
	static class FileLabelProvider extends LabelProvider {
		/*
		 * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
		 */
		public String getText(Object element) {
			if (element instanceof IFile) {
				IPath path=  ((IFile) element).getFullPath();
				return path != null ? path.toString() : ""; //$NON-NLS-1$
			}
			return super.getText(element);
		}
	} 
	
	private String getEditorId(File file) {
		return "org.cs3.pl.editors.PLEditor"; //$NON-NLS-1$
	}
	
	private IEditorInput createEditorInput(File file) {
		IFile workspaceFile= getWorkspaceFile(file);
		if (workspaceFile != null)
			return new FileEditorInput(workspaceFile);
		return new JavaFileEditorInput(file);
	}
	
	private IFile getWorkspaceFile(File file) {
		IWorkspace workspace= ResourcesPlugin.getWorkspace();
		IPath location= new Path(file.getAbsolutePath());
		IFile[] files= workspace.getRoot().findFilesForLocation(location);
		if (files == null || files.length == 0)
			return null;
		if (files.length == 1)
			return files[0];
		return selectWorkspaceFile(files);
	}
	
	private IFile selectWorkspaceFile(IFile[] files) {
		ElementListSelectionDialog dialog= new ElementListSelectionDialog(plugin.getActiveWorkbenchWindow().getShell(), new FileLabelProvider());
		dialog.setElements(files);
		dialog.setTitle("Select Workspace File");
		dialog.setMessage("The selected file is referenced by multiple linked resources in the workspace.\nPlease select the workspace resource you want to use to open the file.");
		if (dialog.open() == Window.OK)
			return (IFile) dialog.getFirstResult();
		return null;
	}
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction, org.eclipse.jface.viewers.ISelection)
	 */
	public void selectionChanged(IAction action, ISelection selection) {
		
	}

	/* (non-Javadoc)
	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
	 */
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// TODO Auto-generated method stub
		
	}

	/**
	 * @param data
	 * @param manager
	 * @return
	 */
	private SourceLocation getLocationForCurrentPredicateInEditor(final PrologElementData data, IPrologClient manager) {
		final SourceLocation location = new PrologHelper(manager)
				.getLocation(data.getLabel(), data
						.getArity(),
						((FileEditorInput) plugin
								.getActiveEditor()
								.getEditorInput())
								.getFile().getFullPath()
								.toString());
		return location;
	} 
	
	
	
}
