package org.cs3.pl.buttons;

import java.util.Hashtable;

import org.cs3.pl.Debug;
import org.cs3.pl.PDTPlugin;
import org.cs3.pl.editors.PLEditor;
import org.cs3.pl.prolog.IPrologClient;
import org.cs3.pl.prolog.PrologElementData;
import org.cs3.pl.prolog.PrologManager;
import org.eclipse.jdt.internal.ui.javaeditor.JavaEditorMessages;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindowActionDelegate;
import org.eclipse.ui.texteditor.ITextEditor;
import org.eclipse.ui.texteditor.TextEditorAction;

/**
 * @see IWorkbenchWindowActionDelegate
 */
public class SpyPointActionDelegate extends TextEditorAction {

	/**
	 *
	 */
	public SpyPointActionDelegate(ITextEditor editor) {
			super(JavaEditorMessages.getResourceBundle(), null, editor);
		}

	/**
	 * @see IWorkbenchWindowActionDelegate#run
	 */
	
	Hashtable spypred = new Hashtable();
	
	public void run()  {
		PDTPlugin.getDefault().getDisplay().asyncExec(new Runnable() {
			public void run() {
				
				PLEditor editor = (PLEditor)PDTPlugin.getDefault().getActiveEditor();
				IPrologClient manager;
				manager = PrologManager.getInstance().getClient();
				String pred;
				try {
					PrologElementData data = editor.getSelectedPrologElement();
					if (data == null) {
						MessageDialog.openInformation(editor.getEditorSite().getShell(),"PDT Plugin", "Cannot locate a predicate at the specified location.");
						return;

					}
					if (data.isModule()) {
						MessageDialog.openInformation(editor.getEditorSite().getShell(),"PDT Plugin", "Cannot spy on a module: " + data.getSignature() + ".");
						return;

					}
					pred = data.getSignature();
					
					if (spypred.get(pred) != null){
						manager.query("nospy("+pred+")");
						spypred.remove(pred);
					}
					else {
						manager.query("spy("+pred+")");
						spypred.put(pred,pred);
					}
				} catch (BadLocationException e) {
					// TODO Auto-generated catch block
					Debug.report(e);
				}
			}
		});
	}

	/**
	 * @param editorPart
	 */
	private void updateOutline(PLEditor editor,String filename) {
		// trigger inputChanged...
		editor.getOutlinePage().getTreeViewer().setInput(editor.getEditorInput());
//		editor.getOutlinePage().getContentProvider().inputChanged(editor.getOutlinePage().getTreeViewer(),null,filename);
	}

	/**
	 * @see IWorkbenchWindowActionDelegate#selectionChanged
	 */
	public void selectionChanged(IAction action, ISelection selection)  {

	}

	/**
	 * @see IWorkbenchWindowActionDelegate#dispose
	 */
	public void dispose()  {
	}


	/* (non-Javadoc)
	 * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction, org.eclipse.ui.IEditorPart)
	 */
	public void setActiveEditor(IAction action, IEditorPart targetEditor) {
		// TODO Auto-generated method stub
		
	}
}
