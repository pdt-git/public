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
 * most of the methods in this class include code that needs to run on the ui thread.
 * 
 * If the calling thread is not the ui thread, this methods will take care of scheduling
 * the respective code using Diplay.asyncExec() for void methods and Display.syncExec for 
 * all others.
 *
 */
public final class UIUtils {
	private abstract static class _SyncReturn implements Runnable {
		public Object rval;

		_SyncReturn() {
			Display display = getDisplay();
			if (Display.getCurrent() != display) {
				display.syncExec(this);
			} else {
				run();
			}
		}
		
		public void run() {
			rval=getRVal();
		}

		abstract Object getRVal();
	}
	public static IFile getFileInActiveEditor() {
		return (IFile) new _SyncReturn(){
			Object getRVal() {
				IEditorPart activeEditor = getActiveEditor();
				FileEditorInput fileEditorInput = ((FileEditorInput) activeEditor
						.getEditorInput());
				IFile file = fileEditorInput.getFile();
				return file;		
			}
		}.rval;
		
	}

	public static Display getDisplay() {
	    return PlatformUI.getWorkbench().getDisplay();
	}

	public static IWorkbenchPage getActivePage() {
		return (IWorkbenchPage) new _SyncReturn(){
			Object getRVal() {
				return PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage();		
			}
		}.rval;
	    
	}

	
	public static IEditorPart getActiveEditor() {
		return (IEditorPart) new _SyncReturn(){
			Object getRVal() {
				IWorkbenchPage page = getActivePage();
		        if(page==null){
		        	return null;
		        }
		        return page.getActiveEditor();		
			}
		}.rval;
	        
	    
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
