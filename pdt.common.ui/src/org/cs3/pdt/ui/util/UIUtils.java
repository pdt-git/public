/*****************************************************************************
 * This file is part of the Prolog Development Tool (PDT)
 * 
 * Author: Lukas Degener (among others) 
 * E-mail: degenerl@cs.uni-bonn.de
 * WWW: http://roots.iai.uni-bonn.de/research/pdt 
 * Copyright (C): 2004-2006, CS Dept. III, University of Bonn
 * 
 * All rights reserved. This program is  made available under the terms 
 * of the Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 * 
 * In addition, you may at your option use, modify and redistribute any
 * part of this program under the terms of the GNU Lesser General Public
 * License (LGPL), version 2.1 or, at your option, any later version of the
 * same license, as long as
 * 
 * 1) The program part in question does not depend, either directly or
 *   indirectly, on parts of the Eclipse framework and
 *   
 * 2) the program part in question does not include files that contain or
 *   are derived from third-party work and are therefor covered by special
 *   license agreements.
 *   
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *   
 * ad 1: A program part is said to "depend, either directly or indirectly,
 *   on parts of the Eclipse framework", if it cannot be compiled or cannot
 *   be run without the help or presence of some part of the Eclipse
 *   framework. All java classes in packages containing the "pdt" package
 *   fragment in their name fall into this category.
 *   
 * ad 2: "Third-party code" means any code that was originaly written as
 *   part of a project other than the PDT. Files that contain or are based on
 *   such code contain a notice telling you so, and telling you the
 *   particular conditions under which they may be used, modified and/or
 *   distributed.
 ****************************************************************************/

package org.cs3.pdt.ui.util;


import org.eclipse.core.resources.IFile;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Plugin;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.ErrorDialog;
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
	
	/**
	 * hint for file option. if true, option values should be in the workspace
	 */
	public final static String IS_WORKSPACE_RESOURCE = "is_workspace_resource";
	
	/**
	 * hint for file options. if set, only files/folders below the given container 
	 * should be legal option values.
	 */
	public final static String ROOT_CONTAINER = "root_container";
	
	/**
	 * hint for file options. if set to true, values should be interpreted
	 * as relative paths.
	 * This typically involves ROOT_CONTAINER, too.
	 * 
	 */
	public final static String RELATIVE = "relative";
	
	
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
	
	public static void displayErrorDialog(final Shell shell, final String title,
			final String msg) {
		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				public void run() {
					displayMessageDialog(shell, title, msg);
				}
			});
			return;
		}
		MessageDialog.openError(shell, title, msg);
	
	}
	
	public static void displayErrorDialog(final ErrorMessageProvider provider, final Shell shell, final int code, final int context, final Exception x){
		
		if (Display.getCurrent() != shell.getDisplay()) {
			shell.getDisplay().asyncExec(new Runnable() {
				public void run() {
					displayErrorDialog(provider,shell, code,context, x);
				}
			});
			return;
		}
		IStatus status = createErrorStatus(provider, x, code);
		String cxMsg=provider.getContextMessage(context);		
		ErrorDialog.openError(shell,"Problem encountered", cxMsg, status);
	}
	public static IStatus createErrorStatus(ErrorMessageProvider provider, Throwable e, int errCode){
		Status status = new Status(Status.ERROR,provider.getId(),errCode,provider.getErrorMessage(errCode),e);
		
		return status;
	}

	public static void logError(final ErrorMessageProvider provider, final int code, final int context, final Exception x){
		IStatus status = createErrorStatus(provider, x, code);		
		Plugin plugin =provider.getPlugin();
		plugin.getLog().log(status);
	}
	public static void logAndDisplayError(final ErrorMessageProvider provider, final Shell shell, final int code, final int context, final Exception x){
		logError(provider, code, context, x);
		displayErrorDialog(provider, shell, code, context, x);
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
