package org.cs3.pdt.internal.actions;

import java.util.Hashtable;
import java.util.ResourceBundle;

import org.cs3.pdt.PDT;
import org.cs3.pdt.PDTPlugin;
import org.cs3.pdt.PDTUtils;
import org.cs3.pdt.core.IPrologProject;
import org.cs3.pdt.core.PDTCore;
import org.cs3.pdt.core.PDTCorePlugin;
import org.cs3.pdt.internal.editors.PLEditor;
import org.cs3.pdt.runtime.PrologRuntimePlugin;
import org.cs3.pdt.ui.util.UIUtils;
import org.cs3.pl.common.Debug;
import org.cs3.pl.metadata.Goal;
import org.cs3.pl.metadata.Predicate;
import org.cs3.pl.prolog.PrologException;
import org.cs3.pl.prolog.PrologSession;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.text.BadLocationException;
import org.eclipse.ui.IFileEditorInput;
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
        super(ResourceBundle.getBundle(PDT.RES_BUNDLE_UI),SpyPointActionDelegate.class.getName(),editor); //$NON-NLS-1$
    }

    /**
     * @see IWorkbenchWindowActionDelegate#run
     */

    Hashtable spypred = new Hashtable();

    public void run() {
        PDTPlugin plugin = PDTPlugin.getDefault();
        UIUtils.getDisplay().asyncExec(new Runnable() {
            public void run() {
                

                PLEditor editor = (PLEditor) UIUtils.getActiveEditor();
                IFileEditorInput editorInput = (IFileEditorInput) editor.getEditorInput();
				IPrologProject plProject;
				try {
					plProject = (IPrologProject) editorInput.getFile().getProject().getNature(PDTCore.NATURE_ID);
				} catch (CoreException e) {
					Debug.report(e);
					throw new RuntimeException(e);
				}
				PrologSession session = null;
                /*FIXME which PIF to use?
                 * actualy we should not decide here. We are setting
                 * a property of the source file, not of any runtime. 
                 * this should be anotated using resource properties or 
                 * something similar and the consult action should care
                 * about this. I am creating an issue, so we do not 
                 * forget about this problem.
                 */
				session = PDTUtils.getActiveConsolePif().getSession();
                String pred;

                Goal data;
                try {
                    data = editor.getSelectedPrologElement();
                } catch (BadLocationException e2) {
                    Debug.report(e2);
                    return;
                }
                if (data == null) {
                    MessageDialog
                            .openInformation(editor.getEditorSite().getShell(),
                                    "PDT Plugin", 
                                    "Cannot locate a predicate at the specified location."); 
                    return;

                }
				Predicate[] p = plProject.getMetaInfoProvider().findPredicates(data);
				//FIXME what about alternatives?
				
                if (p==null||p.length==0) {
                    MessageDialog.openInformation(editor.getEditorSite()
                            .getShell(), "PDT Plugin", 
                            "Cannot find predicate: "+ data //$NON-NLS-2$
                                    + "."); //$NON-NLS-1$
                    return;

                }
				if(p.length>1){
					UIUtils.displayMessageDialog(UIUtils.getActiveEditor().getEditorSite().getShell(),
							"PDT Plugin", "Note: I found more than one predicate matching the signature \n" 
							+ data.getName()+"/"+ data.getArity()
									+ ".\nSorry, Code analysis is still work in progress. " +
											"For now i will ignore all but the first match.");
				}
                pred = p[0].getSignature();

                if (spypred.get(pred) != null) {
                    try {
                        session.queryOnce("nospy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
                    } catch (PrologException e1) {
                        Debug.report(e1);
                        return;
                    }
                    spypred.remove(pred);
                } else {
                    try {
                        session.queryOnce("spy(" + pred + ")"); //$NON-NLS-1$ //$NON-NLS-2$
                    } catch (PrologException e1) {
                        Debug.report(e1);
                        return;
                    }
                    spypred.put(pred, pred);
                }

            }
        });
    }

    



    /**
     * @see IWorkbenchWindowActionDelegate#dispose
     */
    public void dispose() {
    }

  
}