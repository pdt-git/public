package org.cs3.pdt;

import java.io.IOException;

import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.SourceLocation;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.ide.IDE;
import org.eclipse.ui.part.FileEditorInput;

public final class UIUtils {

	public static IFile getFileInActiveEditor() {
		IEditorPart activeEditor = getActiveEditor();
		FileEditorInput fileEditorInput = ((FileEditorInput) activeEditor
				.getEditorInput());
		IFile file = fileEditorInput.getFile();
		return file;
	}

	public static Display getDisplay() {
	    return PlatformUI.getWorkbench().getDisplay();
	}

	public static IWorkbenchPage getActivePage() {
	    return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();
	}

	public static IEditorPart getActiveEditor() {
	    try {
	        IWorkbenchPage page = getActivePage();
	        return page.getActiveEditor();
	    } catch (NullPointerException e) {
	        return null;
	    }
	}

	public static void showSourceLocation(final SourceLocation loc) {
		if(Display.getCurrent()!=getDisplay()){
			getDisplay().asyncExec(new Runnable() {			
				public void run() {
					showSourceLocation(loc);
				}			
			});
			return;
		}
		
	    IFile file=null;   
		IPath fpath= new Path(loc.file);
		IWorkspaceRoot wsRoot = ResourcesPlugin.getWorkspace().getRoot();
		
		//see if it is a workspace path:
		file = wsRoot.getFile(fpath );
		
		boolean showLine;
		if(!loc.isWorkspacePath) {
			try {
				file = PDTUtils.findFileForLocation(loc.file);
			} catch (IOException e) {
				Debug.report(e);
				throw new RuntimeException(e);
			}
		}
	    
	    IEditorPart part;
		
	    try {
	        IWorkbenchPage page=getActivePage();
			part = IDE.openEditor(page, file);
	    } catch (PartInitException e) {
	        Debug.report(e);
	        return;
	    }
	    if (part instanceof PLEditor) {
	        PLEditor editor = (PLEditor) part;
	      
			if(loc.isRowBased)
				editor.gotoLine(loc.line);
			else
				editor.gotoOffset(loc.offset);
	    }
		
	}

	public static void displayMessageDialog(final Shell shell, final String title,
			final String msg) {
		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				public void run() {
					displayMessageDialog(shell, title, msg);
				}
			});
			return;
		}
		MessageDialog.openInformation(shell, title, msg);
	
	}

	public static void setStatusErrorMessage(final String string) {
	    getDisplay().asyncExec(new Runnable() {
	        public void run() {
	            getActiveEditor().getEditorSite().getActionBars()
	                    .getStatusLineManager().setErrorMessage(string);
	        }
	    });
	}

	public static void setStatusMessage(final String string) {
	    getDisplay().asyncExec(new Runnable() {
	        public void run() {
	            getActiveEditor().getEditorSite().getActionBars()
	                    .getStatusLineManager().setMessage(string);
	        }
	    });
	
	}

}
