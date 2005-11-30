package org.cs3.pdt.ui.util;


import org.eclipse.core.resources.IFile;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.FileEditorInput;


/**
 * 
 * FIXME: some of the methods have to be called on UI Thread,
 * others do their own de-syncing. We should agree on one approach or at least
 * document which methods expects to be run on UI THread.
 *
 */
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
	    
	        IWorkbenchPage page = getActivePage();
	        if(page==null){
	        	return null;
	        }
	        return page.getActiveEditor();
	    
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
